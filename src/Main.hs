{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Categorizer
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import qualified Data.ByteString.Lazy as B
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Database
import NewDatabase
import ConnectionPool
import Database.Persist.Postgresql hiding (get)
import Database.SQLite.Simple (Only (Only), close, execute, open, query)
import HtmlGenerators.AllFilesPage
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.HomePage
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.RefineSelectionPage
import Models
import Network.HTTP.Client (Request (redactHeaders))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers (extractTextFromPdf, processPdfFile)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (open)
import Types
import Web.Scotty
import qualified Web.Scotty as Web


addUploadConfig :: FilePath -> Web.Scotty.ActionM ()
addUploadConfig dbPath = do
  filenameRegex <- Web.Scotty.formParam "filenameRegex" :: Web.Scotty.ActionM Text
  startKeyword <- Web.Scotty.formParam "startKeyword" :: Web.Scotty.ActionM Text
  endKeyword <- Web.Scotty.formParam "endKeyword" :: Web.Scotty.ActionM Text
  transactionSourceId <- Web.Scotty.formParam "transactionSourceId" :: Web.Scotty.ActionM Int
  liftIO $ do
    conn <- open dbPath
    execute
      conn
      "INSERT INTO upload_configuration (filename_regex, start_keyword, end_keyword, transaction_source_id) VALUES (?, ?, ?, ?)"
      (filenameRegex, startKeyword, endKeyword, transactionSourceId)
    close conn
  Web.Scotty.redirect "/manage-upload-config"

deleteUploadConfig :: FilePath -> Web.Scotty.ActionM ()
deleteUploadConfig dbPath = do
  configId <- Web.Scotty.param "id" :: Web.Scotty.ActionM Int
  liftIO $ do
    conn <- open dbPath
    execute conn "DELETE FROM upload_configuration WHERE id = ?" (Only configId)
    close conn
  Web.Scotty.redirect "/manage-upload-config"

editUploadConfig :: FilePath -> Web.Scotty.ActionM ()
editUploadConfig dbPath = do
  configId <- Web.Scotty.param "id" :: Web.Scotty.ActionM Int
  filenameRegex <- Web.Scotty.formParam "filenameRegex" :: Web.Scotty.ActionM Text
  startKeyword <- Web.Scotty.formParam "startKeyword" :: Web.Scotty.ActionM Text
  endKeyword <- Web.Scotty.formParam "endKeyword" :: Web.Scotty.ActionM Text
  transactionSourceId <- Web.Scotty.formParam "transactionSourceId" :: Web.Scotty.ActionM Int
  liftIO $ do
    conn <- open dbPath
    execute
      conn
      "UPDATE upload_configuration SET filename_regex = ?, start_keyword = ?, end_keyword = ?, transaction_source_id = ? WHERE id = ?"
      (filenameRegex, startKeyword, endKeyword, transactionSourceId, configId)
    close conn
  Web.Scotty.redirect "/manage-upload-config"

deleteProcessedFile :: FilePath -> Int -> Text -> IO ()
deleteProcessedFile dbPath transactionSourceId filename = do
  conn <- open dbPath
  execute
    conn
    "DELETE FROM transactions WHERE transaction_source_id = ? AND filename = ?"
    (transactionSourceId, filename)
  execute
    conn
    "DELETE FROM processed_files WHERE filename = ?"
    (Only filename)
  close conn

main :: IO ()
main = do
  let dbPath = "transactions.db"

  activeJobs <- newIORef 0

  migratePostgres

  -- todo remove sqlite
  ConnectionPool.initializeDatabase
  Database.initializeDatabase dbPath
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/" $ do
      activeJobs <- liftIO $ readIORef activeJobs
      let banner = if activeJobs > 0 then Just "Job Running" else Nothing
      liftIO $ print banner
      content <- liftIO $ renderHomePage banner
      Web.html content

    post "/setup-upload" $ do
      startKeyword <- Web.Scotty.formParam "startKeyword" :: ActionM T.Text
      endKeyword <- Web.Scotty.formParam "endKeyword" :: ActionM T.Text
      filenamePattern <- Web.Scotty.formParam "filenamePattern" :: ActionM T.Text
      sourceId <- Web.Scotty.formParam "transactionSourceId" :: ActionM Int

      let dbPath = "transactions.db"
      liftIO $ persistUploadConfiguration dbPath startKeyword endKeyword sourceId filenamePattern

      redirect "/"

    post "/upload-template" $ do
      allFiles <- Web.Scotty.files
      case Prelude.lookup "pdfFile" allFiles of
        Nothing -> do
          Web.text "No file with field name 'pdfFile' was uploaded!"
        Just fileInfo -> do
          let uploadedBytes = fileContent fileInfo
          let originalName = decodeUtf8 $ fileName fileInfo

          let tempFilePath = "/tmp/" <> originalName
          liftIO $ B.writeFile (T.unpack tempFilePath) uploadedBytes

          extractedTextOrError <-
            liftIO $ try (extractTextFromPdf (T.unpack tempFilePath)) ::
              ActionM (Either SomeException Text)
          case extractedTextOrError of
            Left err -> do
              Web.text $ "Failed to parse the PDF: " <> TL.pack (show err)
            Right rawText -> do
              let dbPath = "transactions.db"
              newPdfId <- liftIO $ insertPdfRecord dbPath originalName rawText
              redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show newPdfId))

    post "/upload" $ do
      allFiles <- Web.Scotty.files
      case Prelude.lookup "pdfFile" allFiles of
        Nothing -> do
          Web.text "No file with field name 'pdfFile' was uploaded!"
        Just fileInfo -> do
          let uploadedBytes = fileContent fileInfo
          let originalName = decodeUtf8 $ fileName fileInfo

          let tempFilePath = "/tmp/" <> originalName
          liftIO $ B.writeFile (T.unpack tempFilePath) uploadedBytes

          extractedTextOrError <-
            liftIO $ try (extractTextFromPdf (T.unpack tempFilePath)) ::
              ActionM (Either SomeException Text)
          case extractedTextOrError of
            Left err -> do
              Web.text $ "Failed to parse the PDF: " <> TL.pack (show err)
            Right rawText -> do
              let dbPath = "transactions.db"

              -- Try to get upload configuration
              maybeConfig <- liftIO $ getUploadConfiguration dbPath (T.unpack originalName)

              case maybeConfig of
                Just Types.UploadConfiguration {startKeyword, endKeyword, transactionSourceId, filenameRegex} -> do
                  newPdfId <- liftIO $ insertPdfRecord dbPath originalName rawText

                  liftIO $ do
                    modifyIORef activeJobs (+ 1)
                    _ <- Control.Concurrent.Async.async $ do
                      let config = Types.UploadConfiguration {transactionSourceId = transactionSourceId, filenameRegex = filenameRegex, endKeyword = endKeyword, startKeyword = startKeyword}
                      processPdfFile dbPath newPdfId config
                      modifyIORef activeJobs (subtract 1)
                    return ()

                  redirect "/"
                Nothing -> do
                  newPdfId <- liftIO $ insertPdfRecord dbPath originalName rawText
                  redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show newPdfId))

    get "/adjust-transactions/:pdfId" $ do
      let dbPath = "transactions.db"
      pdfId <- pathParam "pdfId"
      transactionSources <- liftIO $ getAllTransactionSources dbPath
      (fileName, rawText) <- liftIO $ fetchPdfRecord dbPath pdfId
      let segments = T.splitOn "\n" rawText

      Web.html $ renderSliderPage pdfId fileName segments transactionSources

    get "/transactions" $ do
      let dbPath = "transactions.db"
      filenames <- liftIO $ getAllFilenames dbPath
      Web.html $ renderAllFilesPage filenames

    get "/transactions/:filename" $ do
      let dbPath = "transactions.db"
      filename <- Web.Scotty.pathParam "filename"
      transactions <- liftIO $ getTransactionsByFilename dbPath filename
      Web.html $ renderTransactionsPage (T.pack filename) transactions

    post "/update-category" $ do
      let dbPath = "transactions.db"
      tId <- Web.Scotty.formParam "transactionId" :: ActionM T.Text
      newCat <- Web.Scotty.formParam "newCategory" :: ActionM T.Text
      fileArg <- Web.Scotty.formParam "filename" :: ActionM T.Text
      liftIO $ updateTransactionCategory dbPath (read $ T.unpack tId) newCat
      redirect $ TL.fromStrict ("/transactions/" <> fileArg)

    post "/update-sankey-config" $ do
      configName <- Web.Scotty.formParam "configName"

      allParams <- Web.Scotty.formParams

      let inputSourceIds = [read $ T.unpack value | (key, value) <- allParams, key == "inputSourceId[]"]
          inputCategoryIds = [read $ T.unpack value | (key, value) <- allParams, key == "inputCategoryId[]"]

      linkageSourceId <- Web.Scotty.formParam "linkageSourceId"
      linkageCategoryId <- Web.Scotty.formParam "linkageCategoryId"
      linkageTargetId <- Web.Scotty.formParam "linkageTargetId"

      let dbPath = "transactions.db"

      inputTransactionSources <- liftIO $ mapM (getTransactionSource dbPath) inputSourceIds
      inputCategories <- liftIO $ mapM (getCategory dbPath) inputCategoryIds

      linkageSource <- liftIO $ getTransactionSource dbPath linkageSourceId
      linkageCategory <- liftIO $ getCategory dbPath linkageCategoryId
      linkageTarget <- liftIO $ getTransactionSource dbPath linkageTargetId

      let inputs = zip inputTransactionSources inputCategories
          linkages = (linkageSource, linkageCategory, linkageTarget)
          newConfig = Types.SankeyConfig {configName, inputs, linkages, mapKeyFunction = sourceName}

      liftIO $ saveSankeyConfig dbPath newConfig
      redirect "/"

    post "/add-upload-config" $ addUploadConfig dbPath
    post "/delete-upload-config/:id" $ deleteUploadConfig dbPath
    post "/edit-upload-config/:id" $ editUploadConfig dbPath

    post "/delete-processed-file" $ do
      filename <- Web.Scotty.formParam "filename"
      transactionSourceId <- Web.Scotty.formParam "transactionSourceId" :: Web.Scotty.ActionM Int
      liftIO $ deleteProcessedFile dbPath transactionSourceId filename
      Web.Scotty.redirect "/manage-processed-files"

    get "/configuration" $ do
      let dbPath = "transactions.db"
      uploaderConfigs <- liftIO $ getUploadConfigurations dbPath
      transactionSources <- liftIO $ getAllTransactionSources dbPath
      sankeyConfig <- liftIO $ loadSankeyConfig dbPath 1
      transactionSources <- liftIO $ getAllTransactionSources dbPath
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource dbPath . sourceId) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      Web.Scotty.html $ renderConfigurationPage sankeyConfig categoriesBySource uploaderConfigs transactionSources

    post "/add-transaction-source" $ do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ addTransactionSource  newSource
      Web.Scotty.redirect "/configuration"

    post "/edit-transaction-source/:id" $ do
      sourceId <- Web.Scotty.param "id" :: Web.Scotty.ActionM Int
      sourceName <- Web.Scotty.formParam "sourceName" :: Web.Scotty.ActionM Text
      liftIO $ updateTransactionSource dbPath sourceId sourceName
      Web.Scotty.redirect "/configuration"

    post "/add-category/:sourceId" $ do
      sourceId <- Web.Scotty.param "sourceId" :: Web.Scotty.ActionM Int
      newCategory <- Web.Scotty.formParam "newCategory" :: Web.Scotty.ActionM Text
      liftIO $ insertCategory dbPath newCategory sourceId
      Web.Scotty.redirect "/configuration"

    post "/edit-category/:id" $ do
      categoryId <- Web.Scotty.param "id" :: Web.Scotty.ActionM Int
      categoryName <- Web.Scotty.formParam "categoryName" :: Web.Scotty.ActionM Text
      liftIO $ updateCategory dbPath categoryId categoryName
      Web.Scotty.redirect "/configuration"
