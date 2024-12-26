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
import GHC.Generics (Generic)
import Data.Text.Encoding (encodeUtf8)
import Database
import System.Environment (getEnv)
import Control.Exception (SomeException, try)

data OpenAIRequest = OpenAIRequest
  { model :: Text
  , prompt :: Text
  , max_tokens :: Int
  , temperature :: Double
  } deriving (Show, Generic)

instance ToJSON OpenAIRequest

newtype OpenAIResponse
  = OpenAIResponse {choices :: [Choice]}
  deriving (Show, Generic)

newtype Choice
  = Choice {text :: Text}
  deriving (Show, Generic)

instance FromJSON OpenAIResponse
instance FromJSON Choice


decodeResponse :: B.ByteString -> IO (Maybe Text)
decodeResponse responseBodyContent =
  case decode responseBodyContent of
    Just OpenAIResponse { choices = (Choice { text = t } : _) } -> return (Just t)
    _ -> return Nothing

classifyTransactions :: Text -> IO (Maybe Text)
classifyTransactions inputPrompt = do
  manager <- newManager tlsManagerSettings
  apiKey <- getEnv "OPENAI_API_KEY" 
  let url = "https://api.openai.com/v1/completions"
      requestBody = encode OpenAIRequest
        { model = "text-davinci-003"
        , prompt = inputPrompt
        , max_tokens = 100
        , temperature = 0.7
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
      let status = responseStatus res
      if statusCode status == 200 
        then decodeResponse (responseBody res)
        else do
          putStrLn $ "Error: HTTP " ++ show (statusCode status)
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
