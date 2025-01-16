{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ExampleFileParser
  ( generateUploadConfiguration,
  )
where

import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Database.Models
import Database.Persist
import OpenAiUtils
import Parsers
import System.FilePath (takeFileName)
import Types

generateUploadConfigPrompt :: Text -> Text -> Text
generateUploadConfigPrompt pdfContent fileName =
  "look at the following PDF content and filename to determine how we can parse more files in the future. "
    <> "keywords should not contain numbers or random strings, look for things that are stable and likely to be"
    <> "in multiple files. the start and end keywords are so we can isolate that months transactions from the file."
    <> " including a bit of data before or after the transaction tables is totally fine. for the filename, try to find one word"
    <> " that might be exclusive"
    <> "filename:"
    <> fileName
    <> "\n\n\n"
    <> "content:"
    <> pdfContent

generateSchema :: Value
generateSchema =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= ("upload_configuration_schema" :: Text),
            "strict" .= True,
            "schema"
              .= object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "filenameRegex" .= object ["type" .= ("string" :: Text)],
                        "startKeyword" .= object ["type" .= ("string" :: Text)],
                        "endKeyword" .= object ["type" .= ("string" :: Text)]
                      ],
                  "required" .= (["filenameRegex", "startKeyword", "endKeyword"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

generateUploadConfiguration ::
  Entity User ->
  Key TransactionSource ->
  Text ->
  IO (Maybe UploadConfiguration)
generateUploadConfiguration user txnSourceId filePath = do
  extractedText <- extractTextFromPdf (unpack filePath)
  let schema = generateSchema
  let prompt = generateUploadConfigPrompt extractedText (pack (takeFileName $ unpack filePath))
  let messages = [ChatMessage {role = "user", content = prompt}]

  response <- makeChatRequest schema messages
  case response of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right responseBodyContent -> decodeChatResponse responseBodyContent txnSourceId (entityKey user)

decodeChatResponse :: B.ByteString -> Key TransactionSource -> Key User -> IO (Maybe UploadConfiguration)
decodeChatResponse responseBodyContent transactionSourceId userId =
  case decode responseBodyContent of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right PartialUploadConfig {..} -> return $ Just UploadConfiguration
              { uploadConfigurationFilenameRegex = Just partialFilenameRegex
              , uploadConfigurationStartKeyword = Just partialStartKeyword
              , uploadConfigurationEndKeyword = Just partialEndKeyword
              , uploadConfigurationTransactionSourceId = transactionSourceId
              , uploadConfigurationUserId = userId
              }
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          print innerJson
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      print responseBodyContent
      return Nothing
