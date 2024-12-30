{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Categorizer (
    categorizeCreditCardTransaction
    , categorizeBankTransaction
    , aggregateByCategory
    , CategorizedCreditCardTransaction(..)
    , CategorizedBankTransaction(..)
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
import Bank (BankRecord (description))


newtype ChatResponse
  = ChatResponse {choices :: [ChatChoice]}
  deriving (Show, Generic)

instance FromJSON ChatResponse

newtype ChatChoice
  = ChatChoice {message :: ChatMessage}
  deriving (Show, Generic)

instance FromJSON ChatChoice

-- i probably want to learn more about newType vs data
newtype CategorizationResponse
  = CategorizationResponse {responseCategory :: Text}
  deriving (Show, Generic)

instance FromJSON CategorizationResponse

data CategorizedTransaction a = CategorizedTransaction
   { transaction :: a
   , category :: Text
   } deriving (Show, Eq, Ord)

type CategorizedCreditCardTransaction = CategorizedTransaction CreditCardTransaction
type CategorizedBankTransaction = CategorizedTransaction BankRecord
type AggregatedTransactions a = Map.Map Text [CategorizedTransaction a]


aggregateByCategory :: [CategorizedTransaction t] -> AggregatedTransactions t
aggregateByCategory = foldr insertTransaction Map.empty
  where
    insertTransaction :: CategorizedTransaction t -> AggregatedTransactions t -> AggregatedTransactions t
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


categorizeTransaction :: FilePath -> Text -> [Text] -> IO Text
categorizeTransaction dbPath description categories = do
    existingCategory <- getCategory dbPath description
    case existingCategory of
        Just category -> return category 
        Nothing -> do
            apiResponse <- classifyTransactions categories description 
            case apiResponse of
                Just category -> do
                    insertTransaction dbPath description category
                    return category
                Nothing -> return "Uncategorized" 


categorizeCreditCardTransaction :: CreditCardTransaction -> FilePath -> [Text] -> IO CategorizedCreditCardTransaction
categorizeCreditCardTransaction creditCardTransaction dbPath categories = do
    category <- categorizeTransaction dbPath (merchantName creditCardTransaction) categories
    return CategorizedTransaction 
        { Categorizer.transaction = creditCardTransaction
        , category = category
        }


categorizeBankTransaction :: BankRecord -> FilePath -> [Text] -> IO CategorizedBankTransaction
categorizeBankTransaction bankRecord dbPath categories = do
    category <- categorizeTransaction dbPath (description bankRecord) categories
    return CategorizedTransaction 
        { Categorizer.transaction = bankRecord
        , category = category
        }

