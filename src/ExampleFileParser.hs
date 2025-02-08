{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ExampleFileParser
  ( generateUploadConfiguration,
    generateAccountAndCategories,
  )
where

import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Database.Models
import Database.Persist
import OpenAiUtils
import System.FilePath (takeFileName)
import Types
import Types (PartialAccountCategoryConfig (PartialAccountCategoryConfig))

generateUploadConfigPrompt :: Text -> Text
generateUploadConfigPrompt pdfContent =
  "look at the following PDF content and filename to determine how we can parse more files in the future. "
    <> "keywords should not contain numbers or random strings, look for things that are stable and likely to be"
    <> " in files from any month. The start and end keywords are so we can isolate that months transactions from the file."
    <> " including a bit of data before or after the transaction tables is totally fine.  Make sure to also select the words"
    <> " in a way that we have access to a full date somewhere."
    <> " be mindful some files have summary sections, and we DO NOT want those sections."
    <> " PLEASE DO NOT INCLUDE SUMMARY SECTIONS!"
    <> "\n\n\n"
    <> "content:"
    <> pdfContent

generateAccountCatPrompt :: Text -> Text
generateAccountCatPrompt pdfContent =
  "Analyze the following PDF content to determine how to categorize it for parsing in the future."
    <> " Your task is to extract a stable category name, classify the document type, and identify relevant transaction categories."
    <> " The categories should be meaningful, stable across different months, and useful for financial analysis."
    <> " Avoid random strings, numbers, or overly generic terms."
    <> " Be especially careful to exclude summary sections—those should NOT be included!"
    <> "\n\n"
    <> "### Expected JSON Output Format:\n"
    <> "{\n"
    <> "  \"name\": \"<Descriptive name of the account type>\",\n"
    <> "  \"kind\": \"Investment\" | \"Card\" | \"Account\",\n"
    <> "  \"categories\": [\"<category 1>\", \"<category 2>\", \"<category 3>\", ... ]\n"
    <> "}\n\n"
    <> "### Important Notes:\n"
    <> "- The `name` field should be a stable identifier (e.g., 'Chase Checking', 'Fidelity Investments').\n"
    <> "- The `kind` must be one of: `investment`, `card`, or `account`.\n"
    <> "- The `categories` should be a list of relevant financial categories (e.g., 'Groceries', 'Travel', 'Utilities').\n"
    <> "### PDF Content for Analysis:\n\n"
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
                      [ "fileIdKeyword" .= object ["type" .= ("string" :: Text)],
                        "startKeyword" .= object ["type" .= ("string" :: Text)],
                        "endKeyword" .= object ["type" .= ("string" :: Text)]
                      ],
                  "required" .= (["fileIdKeyword", "startKeyword", "endKeyword"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

generateAccountCatSchema :: Value
generateAccountCatSchema =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= ("account_category_schema" :: Text),
            "strict" .= True,
            "schema"
              .= object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "name" .= object ["type" .= ("string" :: Text)],
                        "kind"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "enum" .= (["Investment", "Card", "Account"] :: [Text])
                            ],
                        "categories"
                          .= object
                            [ "type" .= ("array" :: Text),
                              "items" .= object ["type" .= ("string" :: Text)]
                            ]
                      ],
                  "required" .= (["name", "kind", "categories"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

decodeChatResponse :: B.ByteString -> Key TransactionSource -> Key User -> IO (Maybe UploadConfiguration)
decodeChatResponse responseBodyContent transactionSourceId userId =
  case decode responseBodyContent of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right PartialUploadConfig {..} ->
          return $
            Just
              UploadConfiguration
                { uploadConfigurationFilenameRegex = Just partialFilenameRegex,
                  uploadConfigurationStartKeyword = Just partialStartKeyword,
                  uploadConfigurationEndKeyword = Just partialEndKeyword,
                  uploadConfigurationTransactionSourceId = transactionSourceId,
                  uploadConfigurationUserId = userId
                }
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          print innerJson
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      print responseBodyContent
      return Nothing

decodeAccountCatChatResponse :: B.ByteString -> Key User -> IO (Maybe PartialAccountCategoryConfig)
decodeAccountCatChatResponse responseBodyContent userId =
  case decode responseBodyContent of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right config -> return config
        Left err -> do
          putStrLn $ "Failed to decode inner JSON: " <> err
          print innerJson
          return Nothing
    _ -> do
      putStrLn "Failed to decode top-level JSON."
      print responseBodyContent
      return Nothing

generateAccountAndCategories ::
  Entity User ->
  Text ->
  IO (Maybe PartialAccountCategoryConfig)
generateAccountAndCategories user extractedText = do
  let schema = generateAccountCatSchema
  let prompt = generateAccountCatPrompt extractedText
  print prompt
  let messages = [ChatMessage {role = "user", content = prompt}]

  response <- makeChatRequest schema messages
  print response
  case response of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right responseBodyContent -> decodeAccountCatChatResponse responseBodyContent (entityKey user)

generateUploadConfiguration ::
  Entity User ->
  Key TransactionSource ->
  Text ->
  IO (Maybe UploadConfiguration)
generateUploadConfiguration user txnSourceId extractedText = do
  let schema = generateSchema
  let prompt = generateUploadConfigPrompt extractedText
  print prompt
  let messages = [ChatMessage {role = "user", content = prompt}]

  response <- makeChatRequest schema messages
  print response
  case response of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right responseBodyContent -> decodeChatResponse responseBodyContent txnSourceId (entityKey user)
