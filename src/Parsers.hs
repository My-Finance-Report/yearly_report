{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parsers (
    extractTextFromPdf
    , processPdfFile
    , TransactionKind(..)
) where

import Data.Text (Text)
import CreditCard 
import Database
import Data.Aeson 
import OpenAiUtils 
import Control.Exception
import System.Process (readProcess)
import qualified Data.Text as T
import Data.Aeson.KeyMap (mapMaybe)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding (encodeUtf8)
import System.FilePath (takeFileName)


data PdfParseException = PdfParseException Text deriving (Show)

instance Exception PdfParseException

data TransactionsWrapper = TransactionsWrapper
  { transactions :: [Transaction]
  } deriving (Show, Generic)

instance FromJSON TransactionsWrapper


generatePdfParsingPrompt :: Text -> Text
generatePdfParsingPrompt pdfContent =
    "Parse the following PDF content into a JSON array of transactions. " <>
    "Each transaction should have the fields: 'transactionDate', 'postingDate', 'description', and 'amount'.\n\n" <>
    pdfContent


-- TODO really we want to unify this with the type that we are parsing into in the api call
generateTransactionSchema :: Value
generateTransactionSchema =
    object
        [ "type" .= ("json_schema" :: Text)
        , "json_schema" .= object
            [ "name" .= ("cc_transaction_schema" :: Text)
            , "strict" .= True
            , "schema" .= object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "transactions" .= object
                        [ "type" .= ("array" :: Text)
                        , "items" .= object
                            [ "type" .= ("object" :: Text)
                            , "properties" .= object
                                [ "transactionDate" .= object [ "type" .= ("string" :: Text) ]
                                , "postingDate" .= object [ "type" .= ("string" :: Text) ]
                                , "description" .= object [ "type" .= ("string" :: Text) ]
                                , "amount" .= object [ "type" .= ("number" :: Text) ]
                                ]
                            , "required" .= (["transactionDate", "postingDate", "description", "amount"] :: [Text])
                            , "additionalProperties" .= False
                            ]
                        ]
                    ]
                , "required" .= (["transactions"] :: [Text])
                , "additionalProperties" .= False
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
    Just ChatResponse { choices = (ChatChoice { message = ChatMessage { content = innerJson } } : _) } -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right (TransactionsWrapper { transactions = parsedTransactions }) -> return (Just parsedTransactions)
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
    let 
        (_, afterTransactions) = T.breakOn  pdfText keyword
    in afterTransactions

trimTrailingText :: Text -> Text -> Text
trimTrailingText pdfText keyword =
    let 
        (beforeTransactions, _) = T.breakOn  keyword pdfText
    in beforeTransactions


getKeywords :: TransactionKind -> (Text, Text)
getKeywords BankKind = 
    ( "Bank Trans Date   Post Date   Description"
    , "Nov 22       Nov 25      BANK DESCRIPTION END KEYWORD"
    )
getKeywords CreditCardKind = 
    ( "Trans Date   Post Date   Description"
    , "Nov 22       Nov 25      OAKHURST FOOD FUELOAKHURSTCA                                                                               $"
    )



extractTransactionsFromPdf :: FilePath -> TransactionKind -> IO [Transaction]
extractTransactionsFromPdf pdfPath transactionKind = do

    let (startKeyword, endKeyword) = getKeywords transactionKind

    rawText <- extractTextFromPdf pdfPath
    let trimmedText = trimTrailingText (trimLeadingText rawText startKeyword ) endKeyword
    parsedTransactions <- parseRawTextToJson trimmedText
    case parsedTransactions of
        Nothing -> throwIO $ PdfParseException "Failed to parse transactions from extracted text."
        Just transactions -> return transactions


-- TODO extract the existing rows from the DB if they have already been processed
processPdfFile :: FilePath -> FilePath -> TransactionKind -> IO [Transaction]
processPdfFile dbPath pdfPath transactionKind = do
    let filename = takeFileName pdfPath

    alreadyProcessed <- isFileProcessed dbPath (T.pack filename)
    if alreadyProcessed
        then do
            putStrLn $ "File '" ++ filename ++ "' has already been processed."
            return [] 
        else do
            putStrLn $ "Processing file: " ++ filename
            result <- try (extractTransactionsFromPdf pdfPath transactionKind) :: IO (Either SomeException [Transaction])
            case result of
                Left err -> do
                    putStrLn $ "Error processing file '" ++ filename ++ "': " ++ show err
                    return [] 
                Right transactions -> do
                    markFileAsProcessed dbPath (T.pack filename)
                    putStrLn $ "Extracted " ++ show (length transactions) ++ " transactions from '" ++ filename ++ "'."
                    return transactions 

