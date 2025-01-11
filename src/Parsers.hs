{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( extractTextFromPdf,
    processPdfFile,
  )
where

import Categorizer (categorizeTransaction)
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson hiding (Key)
import Data.Aeson.KeyMap (mapMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Database
import Database.Persist
import GHC.Generics (Generic)
import Models
import OpenAiUtils
import System.FilePath (takeFileName)
import System.Process (readProcess)
import Types

generatePdfParsingPrompt :: Text -> Text
generatePdfParsingPrompt pdfContent =
  "Parse the following PDF content into a JSON array of transactions. "
    <> "structure the dates as MM/DD/YYYY"
    <> "Each transaction should have the fields: 'transactionDate', 'description', and 'amount'.\n\n"
    <> pdfContent

-- TODO really we want to unify this with the type that we are parsing into in the api call
generateTransactionSchema :: Value
generateTransactionSchema =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= ("transaction_schema" :: Text),
            "strict" .= True,
            "schema"
              .= object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "transactions"
                          .= object
                            [ "type" .= ("array" :: Text),
                              "items"
                                .= object
                                  [ "type" .= ("object" :: Text),
                                    "properties"
                                      .= object
                                        [ "partialTransactionDateOfTransaction" .= object ["type" .= ("string" :: Text)],
                                          "partialTransactionDescription" .= object ["type" .= ("string" :: Text)],
                                          "partialTransactionKind"
                                            .= object
                                              [ "type" .= ("string" :: Text),
                                                "enum" .= (["Withdrawal", "Deposit"] :: [Text])
                                              ],
                                          "partialTransactionAmount" .= object ["type" .= ("number" :: Text)]
                                        ],
                                    "required" .= (["partialTransactionDateOfTransaction", "partialTransactionDescription", "partialTransactionAmount", "partialTransactionKind"] :: [Text]),
                                    "additionalProperties" .= False
                                  ]
                            ]
                      ],
                  "required" .= (["transactions"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

parseRawTextToJson :: Text -> IO (Maybe [PartialTransaction])
parseRawTextToJson pdfContent = do
  let inputPrompt = generatePdfParsingPrompt pdfContent
  let schema = generateTransactionSchema

  let messages = [ChatMessage {role = "user", content = inputPrompt}]

  response <- makeChatRequest schema messages
  case response of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right responseBodyContent -> decodeChatResponse responseBodyContent

decodeChatResponse :: B.ByteString -> IO (Maybe [PartialTransaction])
decodeChatResponse responseBodyContent =
  case decode responseBodyContent of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right (TransactionsWrapper {transactions = parsedTransactions}) -> return (Just parsedTransactions)
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          print innerJson
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      print responseBodyContent
      return Nothing

-- Extract text from PDF, throwing an exception on failure
extractTextFromPdf :: FilePath -> IO Text
extractTextFromPdf pdfPath = do
  let command = "pdftotext"
      args = ["-layout", pdfPath, "-"]
  result <- try (readProcess command args "") :: IO (Either SomeException String)
  case result of
    Left err -> throwIO $ PdfParseException $ "Failed to extract text from PDF: " <> T.pack (show err)
    Right output -> return $ T.pack output

trimLeadingText :: Text -> Maybe Text -> Text
trimLeadingText pdfText keyword =
  case keyword of
    Just key ->
      let (_, afterTransactions) = T.breakOn key pdfText
       in T.drop (T.length key) afterTransactions -- Drop the keyword itself
    Nothing -> pdfText -- If no keyword, return the original text

trimTrailingText :: Text -> Maybe Text -> Text
trimTrailingText pdfText keyword =
  case keyword of
    Just key ->
      let (beforeTransactions, _) = T.breakOn key pdfText
       in beforeTransactions
    Nothing -> pdfText -- If no keyword, return the original text

extractTransactionsFromLines :: Text -> TransactionSource -> Maybe Text -> Maybe Text -> IO [PartialTransaction]
extractTransactionsFromLines rawText transactionSource startKeyword endKeyword = do
  let trimmedLines = trimTrailingText (trimLeadingText rawText startKeyword) endKeyword

  parsedTransactions <- parseRawTextToJson trimmedLines
  case parsedTransactions of
    Nothing -> throwIO $ PdfParseException "Failed to parse transactions from extracted text."
    Just transactions -> return transactions

processPdfFile :: Entity User -> Key UploadedPdf -> Entity UploadConfiguration -> IO [CategorizedTransaction]
processPdfFile user pdfId config = do
  uploadedFile <- liftIO $ fetchPdfRecord pdfId
  let filename = uploadedPdfFilename uploadedFile
  alreadyProcessed <- liftIO $ isFileProcessed filename

  transactionSource <- getTransactionSource (uploadConfigurationTransactionSourceId $ entityVal config)

  if alreadyProcessed
    then do
      putStrLn $ "File '" ++ show pdfId ++ "' has already been processed."
      getTransactionsByFileId user pdfId
    else do
      putStrLn $ "Processing file: " ++ T.unpack filename

      result <- try (extractTransactionsFromLines (uploadedPdfRawContent uploadedFile) (entityVal transactionSource) (uploadConfigurationStartKeyword $ entityVal config) (uploadConfigurationEndKeyword $ entityVal config)) :: IO (Either SomeException [PartialTransaction])
      case result of
        Left err -> do
          let errorMsg = "Error processing file '" ++ T.unpack filename ++ "': " ++ show err
          putStrLn errorMsg
          return []
        Right transactions -> do
          -- Categorize and store transactions
          categorizedTransactions <- mapM (\txn -> categorizeTransaction user txn pdfId (entityKey transactionSource)) transactions
          markFileAsProcessed filename
          putStrLn $ "Extracted and categorized " ++ show (length categorizedTransactions) ++ " transactions from '" ++ T.unpack filename ++ "'."
          return categorizedTransactions
