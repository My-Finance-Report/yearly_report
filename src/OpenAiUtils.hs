{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAiUtils
  ( makeChatRequest
  , ChatMessage(..)
  , ChatRequest(..)
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as B
import Control.Exception (SomeException, try)
import System.Environment (getEnv)
import GHC.Generics

data ChatRequest = ChatRequest
  { model :: Text
  , messages :: [ChatMessage]
  , response_format :: Value
  , temperature :: Double
  , max_tokens :: Int
  } deriving (Show, Generic)

instance ToJSON ChatRequest

data ChatMessage = ChatMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Generic)

instance ToJSON ChatMessage
instance FromJSON ChatMessage



makeChatRequest ::  Value -> [ChatMessage] -> IO (Either String B.ByteString)
makeChatRequest schema messages = do

  apiKey <- getEnv "OPENAI_API_KEY"
  manager <- newManager tlsManagerSettings
  let url = "https://api.openai.com/v1/chat/completions"
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
    Left err -> return $ Left (show err)
    Right res -> return $ Right (responseBody res)
