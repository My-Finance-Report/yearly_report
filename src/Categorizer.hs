{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Categorizer (
    categorizeTransaction
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
  = CategorizationResponse {category :: Text}
  deriving (Show, Generic)

instance FromJSON CategorizationResponse

-- Function to classify transactions using the Chat API with response_format
classifyTransactions :: Text -> IO (Maybe Text)
classifyTransactions inputPrompt = do
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
                    [ "category" .= object
                        [ "type" .= ("string" :: Text)
                        , "enum" .= (["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food"] :: [Text])
                        , "description" .= ("The category of the item." :: Text)
                        ]
                    ]
                    , "required" .= (["category"] :: [Text])
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
    Just ChatResponse { choices = (ChatChoice { message = ChatMessage { content = innerJson } } : _) } ->
      -- Parse the inner JSON from `content`
        case decode (BL.fromStrict (encodeUtf8  innerJson)) of
        Just CategorizationResponse { category = c } -> return (Just c)
        Nothing -> do
          putStrLn "Failed to decode inner JSON."
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      return Nothing







generatePrompt :: [Text] -> [Text] -> Text
generatePrompt categories transactions =
  let categoryList = "Here is a list of categories: " <> T.pack (show categories) <> ".\n"
      transactionList = "Assign each transaction to the most appropriate category:\n" <>
                        T.concat [T.pack (show i) <> ". \"" <> t <> "\"\n" | (i, t) <- zip [1 :: Int ..] transactions]
  in categoryList <> transactionList <> "\nReturn the category for each transaction."

categorizeTransaction :: FilePath -> Text -> [Text] -> IO Text
categorizeTransaction dbPath description categories = do
    -- Check if the category already exists in the DB
    existingCategory <- getCategory dbPath description
    case existingCategory of
        Just category -> return category -- Use cached result
        Nothing -> do
            -- Generate a prompt and get the category from OpenAI
            let prompt = generatePrompt categories [description]
            apiResponse <- classifyTransactions prompt
            case apiResponse of
                Just category -> do
                    -- Save the result to the database
                    insertTransaction dbPath description category
                    return category
                Nothing -> return "Uncategorized" -- Handle API failure
