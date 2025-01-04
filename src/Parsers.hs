{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( extractTextFromPdf,
    processPdfFile,
  )
where

import Categorizer (categorizeTransaction)
import Control.Exception
import Data.Aeson
import Data.Aeson.KeyMap (mapMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database
import GHC.Generics (Generic)
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
                                        [ "transactionDate" .= object ["type" .= ("string" :: Text)],
                                          "description" .= object ["type" .= ("string" :: Text)],
                                          "kind"
                                            .= object
                                              [ "type" .= ("string" :: Text),
                                                "enum" .= (["Withdrawal", "Deposit"] :: [Text])
                                              ],
                                          "amount" .= object ["type" .= ("number" :: Text)]
                                        ],
                                    "required" .= (["transactionDate", "description", "amount", "kind"] :: [Text]),
                                    "additionalProperties" .= False
                                  ]
                            ]
                      ],
                  "required" .= (["transactions"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

parseRawTextToJson :: Text -> IO (Maybe [Transaction])
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

decodeChatResponse :: B.ByteString -> IO (Maybe [Transaction])
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

trimLeadingText :: Text -> Text -> Text
trimLeadingText pdfText keyword =
  let (_, afterTransactions) = T.breakOn keyword pdfText
   in afterTransactions

trimTrailingText :: Text -> Text -> Text
trimTrailingText pdfText keyword =
  let (beforeTransactions, _) = T.breakOn keyword pdfText
   in beforeTransactions

-- TODO these come from the DB
getKeywords :: TransactionSource -> (Text, Text)
getKeywords transaction =
  ( "Transaction history",
    "Ending balance"
  )

extractTransactionsFromPdf :: FilePath -> TransactionSource -> IO [Transaction]
extractTransactionsFromPdf pdfPath transactionSource = do
  let (startKeyword, endKeyword) = getKeywords transactionSource

  rawText <- extractTextFromPdf pdfPath
  let trimmedText = trimTrailingText (trimLeadingText rawText startKeyword) endKeyword
  print trimmedText
  parsedTransactions <- parseRawTextToJson trimmedText
  case parsedTransactions of
    Nothing -> throwIO $ PdfParseException "Failed to parse transactions from extracted text."
    Just transactions -> return transactions

processPdfFile :: FilePath -> FilePath -> TransactionSource -> IO [CategorizedTransaction]
processPdfFile dbPath pdfPath transactionSource = do
  let filename = takeFileName pdfPath

  alreadyProcessed <- isFileProcessed dbPath (T.pack filename)

  if alreadyProcessed
    then do
      putStrLn $ "File '" ++ filename ++ "' has already been processed."
      trans <- getAllTransactions dbPath filename
      print trans
      return trans
    else do
      putStrLn $ "Processing file: " ++ filename
      result <- try (extractTransactionsFromPdf pdfPath transactionSource) :: IO (Either SomeException [Transaction])
      case result of
        Left err -> do
          putStrLn $ "Error processing file '" ++ filename ++ "': " ++ show err
          return []
        Right transactions -> do
          categorizedTransactions <- mapM (\txn -> categorizeTransaction txn dbPath filename transactionSource) transactions
          markFileAsProcessed dbPath (T.pack filename)
          putStrLn $ "Extracted and categorized " ++ show (length categorizedTransactions) ++ " transactions from '" ++ filename ++ "'."
          return categorizedTransactions
