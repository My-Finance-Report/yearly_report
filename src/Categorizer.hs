{-# LANGUAGE OverloadedStrings #-}

module Categorizer
  ( categorizeTransaction,
  )
where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Database.Category
import Database.Database
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey)
import GHC.Generics (Generic)
import Models
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

classifyTransactions :: Map.Map Text (Entity Category) -> Text -> IO (Maybe (Entity Models.Category))
classifyTransactions categoryMap description = do
  let categories = Map.keys categoryMap
      inputPrompt = generatePrompt categories description
      schema = generateSchema categories
      messages = [ChatMessage {role = "user", content = inputPrompt}]
  response <- makeChatRequest schema messages
  case response of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right responseBodyContent -> do
      categoryName <- decodeCategorizationResponse responseBodyContent
      case categoryName of
        Just categoryName ->
          case Map.lookup categoryName categoryMap of
            Just category -> return $ Just category
            Nothing -> do
              putStrLn $ "Error: API returned unknown category: " ++ T.unpack categoryName
              return Nothing
        Nothing -> do
          putStrLn "Error: API returned unknown category "
          return Nothing

categorizeTransactionInner ::
  (MonadIO m) =>
  Entity User ->
  Text ->
  UTCTime ->
  Key TransactionSource ->
  m (Entity Category)
categorizeTransactionInner user description day transactionSourceId = do
  categories <- liftIO $ getCategoriesBySource user transactionSourceId
  let categoryMap = Map.fromList [(categoryName (entityVal cat), cat) | cat <- categories]
  apiResponse <- liftIO $ classifyTransactions categoryMap description
  case apiResponse of
    Just category -> return category
    Nothing -> liftIO $ throwIO $ PdfParseException "Unable to properly categorize"

categorizeTransaction ::
  (MonadIO m) =>
  Entity User ->
  PartialTransaction ->
  Key UploadedPdf ->
  Key TransactionSource ->
  m CategorizedTransaction
categorizeTransaction user transaction uploadedPdfKey transactionSourceId = do
  -- Categorize the transaction
  let txnDsc = partialTransactionDescription transaction
  let dateOfTransaction = partialTransactionDateOfTransaction transaction

  categoryEntity <- categorizeTransactionInner user txnDsc dateOfTransaction transactionSourceId

  let categorizedTransaction =
        CategorizedTransaction
          { transactionId = Nothing,
            transaction =
              Transaction
                { transactionDescription = txnDsc,
                  transactionAmount = partialTransactionAmount transaction,
                  transactionKind = parseTransactionKind $ partialTransactionKind transaction,
                  transactionDateOfTransaction = dateOfTransaction,
                  transactionUploadedPdfId = Just uploadedPdfKey,
                  transactionCategoryId = entityKey categoryEntity,
                  transactionTransactionSourceId = transactionSourceId
                },
            category = entityVal categoryEntity
          }

  -- Insert the transaction and get the new ID
  newId <- liftIO $ insertTransaction user categorizedTransaction uploadedPdfKey

  -- Return the updated CategorizedTransaction with the assigned ID
  return $
    categorizedTransaction
      { transactionId = Just newId
      }
