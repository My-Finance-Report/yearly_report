{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Categorizer (
    categorizeTransaction
    , aggregateByCategory
    , CategorizedTransaction(..)
    , AggregatedTransactions 
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Network.HTTP.Types.Status

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Data.Text.Encoding (encodeUtf8)
import Database
import System.Environment (getEnv)
import Control.Exception (SomeException, try)
import CreditCard
import OpenAiUtils
import Data.Time (Day,  formatTime, defaultTimeLocale)
import System.FilePath (takeFileName)



-- i probably want to learn more about newType vs data
newtype CategorizationResponse
  = CategorizationResponse {responseCategory :: Text}
  deriving (Show, Generic)

instance FromJSON CategorizationResponse

data CategorizedTransaction  = CategorizedTransaction
   { transaction :: Transaction
   , category :: Text
   } deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]



getCreditCardSource:: Text
getCreditCardSource= 
  "credit_card"

getBankSource:: Text
getBankSource= 
  "bank"



aggregateByCategory :: [CategorizedTransaction ] -> AggregatedTransactions
aggregateByCategory = foldr insertTransaction Map.empty
  where
    insertTransaction :: CategorizedTransaction  -> AggregatedTransactions  -> AggregatedTransactions 
    insertTransaction categorizedTransaction acc =
        let cat = category categorizedTransaction
        in Map.insertWith (++) cat [categorizedTransaction] acc


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


decodeCategorizationResponse :: B.ByteString -> IO (Maybe Text)
decodeCategorizationResponse responseBodyContent =
  case decode responseBodyContent of
    Just ChatResponse { choices = (ChatChoice { message = ChatMessage { content = innerJson } } : _) } -> do
      case eitherDecode (BL.fromStrict $ encodeUtf8 innerJson) of
        Right CategorizationResponse { responseCategory = c } -> return (Just c)
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      return Nothing


generateSchema :: [Text] -> Value
generateSchema categories = 
      object
                [ "type" .= ("json_schema" :: Text)
                , "json_schema" .= object
                [ "name" .= ("category_schema" :: Text)
                , "strict" .= True
                , "schema" .= object
                    [ "type" .= ("object" :: Text)
                    , "properties" .= object
                    [ "responseCategory" .= object
                        [ "type" .= ("string" :: Text)
                        , "enum" .= categories
                        , "description" .= ("The category of the item." :: Text)
                        ]
                    ]
                    , "required" .= (["responseCategory"] :: [Text])
                    , "additionalProperties" .= False
                    ]
                ]
                ]



generatePrompt :: [Text] -> Text -> Text
generatePrompt categories transaction =
  let categoryList = "Here is a list of categories: " <> T.pack (show categories) <> ".\n"
  in categoryList <> "Assign the transaction to the most appropriate category:\n" <> transaction <> "\nReturn the category for the transaction."

categorizeTransactionInner :: FilePath -> Text -> [Text] -> Text ->Text -> Text-> IO Text
categorizeTransactionInner dbPath description categories day source filename = do
    existingCategory <- getCategory dbPath description
    case existingCategory of
        Just category -> return category 
        Nothing -> do
            apiResponse <- classifyTransactions categories description 
            case apiResponse of
                Just category -> do
                    insertTransaction dbPath description category day source filename
                    return category
                Nothing -> return "Uncategorized" 


categorizeTransaction :: Transaction -> FilePath -> [Text] -> Text-> IO  CategorizedTransaction
categorizeTransaction creditCardTransaction dbPath categories filename = do
    category <- categorizeTransactionInner dbPath (CreditCard.description creditCardTransaction) categories (transactionDate creditCardTransaction) getCreditCardSource filename
    return CategorizedTransaction 
        { Categorizer.transaction = creditCardTransaction
        , category = category
        }
