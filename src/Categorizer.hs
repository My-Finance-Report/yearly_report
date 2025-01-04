{-# LANGUAGE OverloadedStrings #-}

module Categorizer
  ( categorizeTransaction,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Database
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import OpenAiUtils
import System.Environment (getEnv)
import System.FilePath (takeFileName)
import Types

decodeCategorizationResponse :: B.ByteString -> IO (Maybe Text)
decodeCategorizationResponse responseBodyContent =
  case decode responseBodyContent of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (BL.fromStrict $ encodeUtf8 innerJson) of
        Right CategorizationResponse {responseCategory = c} -> return (Just c)
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      return Nothing

generateSchema :: [Text] -> Value
generateSchema categories =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= ("category_schema" :: Text),
            "strict" .= True,
            "schema"
              .= object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "responseCategory"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "enum" .= categories,
                              "description" .= ("The category of the item." :: Text)
                            ]
                      ],
                  "required" .= (["responseCategory"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

generatePrompt :: [Text] -> Text -> Text
generatePrompt categories transaction =
  let categoryList = "Here is a list of categories: " <> T.pack (show categories) <> ".\n"
   in categoryList <> "Assign the transaction to the most appropriate category:\n" <> transaction <> "\nReturn the category for the transaction."

classifyTransactions :: [Text] -> Text -> IO (Maybe Text)
classifyTransactions categories description = do
  let inputPrompt = generatePrompt categories description
      schema = generateSchema categories
      messages = [ChatMessage {role = "user", content = inputPrompt}]
  response <- makeChatRequest schema messages
  case response of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right responseBodyContent -> decodeCategorizationResponse responseBodyContent

categorizeTransactionInner :: FilePath -> Text -> Day -> Int -> IO Text
categorizeTransactionInner dbPath description day transactionId = do
  categories <- getCategoriesBySource dbPath transactionId
  apiResponse <- classifyTransactions (Prelude.map categoryName categories) description
  case apiResponse of
    Just category -> do
      return category
    Nothing -> return "Uncategorized"

categorizeTransaction :: Transaction -> FilePath -> FilePath -> TransactionSource -> IO CategorizedTransaction
categorizeTransaction creditCardTransaction dbPath filename transactionSource = do
  cat <-
    categorizeTransactionInner
      dbPath
      (description creditCardTransaction)
      (transactionDate creditCardTransaction)
      (sourceId transactionSource)

  let partialTx =
        CategorizedTransaction
          { transactionId = Nothing,
            transaction = creditCardTransaction,
            category = cat,
            transactionSource = transactionSource
          }

  newId <- insertTransaction dbPath partialTx filename

  let finalizedTx = partialTx {transactionId = Just newId}
  return finalizedTx
