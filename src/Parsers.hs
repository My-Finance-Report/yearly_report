{-# LANGUAGE OverloadedStrings #-}

module Parsers (
    extractTextFromPdf
    , extractTransactionsFromPdf
) where

import Data.Text (Text)
import CreditCard (CreditCardTransaction)
import Data.Aeson 
import OpenAiUtils (ChatMessage(..), makeChatRequest)
import System.Environment (getEnv)
import Control.Exception
import System.Process (readProcess)
import qualified Data.Text as T
import Data.Aeson.KeyMap (mapMaybe)

generatePdfParsingPrompt :: Text -> Text
generatePdfParsingPrompt pdfContent =
    "Parse the following PDF content into a JSON array of transactions. " <>
    "Each transaction should have the fields: 'transactionDate', 'postingDate', 'description', and 'amount'.\n\n" <>
    pdfContent


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
                                , "amount" .= object [ "type" .= ("string" :: Text) ]
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


parseRawTextToJson :: Text -> IO (Maybe [CreditCardTransaction])
parseRawTextToJson pdfContent = do
    let inputPrompt = generatePdfParsingPrompt pdfContent
    let schema = generateTransactionSchema

    apiKey <- getEnv "OPENAI_API_KEY"

    let messages = [ ChatMessage { role = "user", content = inputPrompt } ]

    response <- makeChatRequest schema messages
    case response of
        Left err -> do
            putStrLn $ "Error: " ++ err
            return Nothing
        Right responseBodyContent -> do
            case decode responseBodyContent of
                Just transactions -> return (Just transactions)
                Nothing -> do
                    putStrLn "Failed to decode JSON response."
                    return Nothing


extractTextFromPdf :: FilePath -> IO (Either String Text)
extractTextFromPdf pdfPath = do
    let command = "pdftotext"
        args = ["-layout", pdfPath, "-"]
    result <- try (readProcess command args "") :: IO (Either SomeException String)
    case result of
        Left err -> return $ Left (show err)
        Right output -> return $ Right (T.pack output)


-- TODO, manually trim the credit card PDFs so that it doesn't require as much API useage
extractTransactionsFromPdf :: FilePath -> IO (Either String [CreditCardTransaction])
extractTransactionsFromPdf pdfPath = do
    rawTextResult <- extractTextFromPdf pdfPath
    case rawTextResult of
        Left err -> return $ Left $ "Failed to extract text from PDF: " ++ err
        Right rawText -> do
            jsonResult <- parseRawTextToJson rawText
            case jsonResult of
                Nothing -> return $ Left "Failed to parse transactions from extracted text."
                Just transactions -> return $ Right transactions