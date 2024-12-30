{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Categorizer (
    categorizeCreditCardTransaction
    , categorizeBankTransaction
    , CategorizedCreditCardTransaction(..)
    , CategorizedBankTransaction(..)
    , CategorizedTransaction(..)
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Network.HTTP.Types.Status
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
import Bank (BankRecord (description))


data ChatMessage = ChatMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Generic)

instance ToJSON ChatMessage
instance FromJSON ChatMessage

data ChatRequest = ChatRequest
  { model :: Text
  , response_format :: Value 
  , messages :: [ChatMessage]
  , max_tokens :: Int
  , temperature :: Double
  } deriving (Show, Generic)

instance ToJSON ChatRequest

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

-- Function to classify transactions using the Chat API with response_format
classifyTransactions :: [Text] ->Text -> IO (Maybe Text)
classifyTransactions categories description = do
  let inputPrompt = generatePrompt categories description
  manager <- newManager tlsManagerSettings
  apiKey <- getEnv "OPENAI_API_KEY"
  let url = "https://api.openai.com/v1/chat/completions"
      messages = [ ChatMessage { role = "user", content = "categorize this expense:\n\n" <> inputPrompt } ]

      schema = object
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
      
      requestBody = encode ChatRequest
        { model = "gpt-4o"
        , messages = messages
        , response_format = schema
        , temperature = 1
        , max_tokens = 2048
        }
  initialRequest <- parseRequest $ T.unpack url
  let request = initialRequest
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> encodeUtf8 (T.pack apiKey))
            ]
        , requestBody = RequestBodyLBS requestBody
        }
  response <- try (httpLbs request manager) :: IO (Either SomeException (Response B.ByteString))
  case response of
    Left err -> do
      print err
      return Nothing
    Right res -> do
      let responseBodyContent = responseBody res
      decodeCategorizationResponse responseBodyContent

decodeCategorizationResponse :: B.ByteString -> IO (Maybe Text)
decodeCategorizationResponse responseBodyContent =
  case decode responseBodyContent of
    Just ChatResponse { choices = (ChatChoice { message = ChatMessage { content = innerJson } } : _) } -> do
      -- Step 1: Parse the escaped JSON from `content`
      case eitherDecode (BL.fromStrict $ encodeUtf8 innerJson) of
        Right CategorizationResponse { responseCategory = c } -> return (Just c)
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      return Nothing


generatePrompt :: [Text] -> Text -> Text
generatePrompt categories transaction =
  let categoryList = "Here is a list of categories: " <> T.pack (show categories) <> ".\n"
  in categoryList <> "Assign the transaction to the most appropriate category:\n" <> transaction <> "\nReturn the category for the transaction."


categorizeTransaction :: FilePath -> Text -> [Text] -> IO Text
categorizeTransaction dbPath description categories = do
    -- Check if the category already exists in the DB
    existingCategory <- getCategory dbPath description
    case existingCategory of
        Just category -> return category -- Use cached result
        Nothing -> do
            -- Generate a prompt and get the category from OpenAI
            apiResponse <- classifyTransactions categories description 
            case apiResponse of
                Just category -> do
                    -- Save the result to the database
                    insertTransaction dbPath description category
                    return category
                Nothing -> return "Uncategorized" -- Handle API failure


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

