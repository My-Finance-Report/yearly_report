{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Categorizer
import ConnectionPool
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as B
import Data.HList
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Database
import Database.Persist.Postgresql hiding (get)
import GHC.Generics (Generic)
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
import Sankey
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (open)
import Types
import Web.Scotty
import qualified Web.Scotty as Web

data Matrix = Matrix
  { columnHeaders :: [T.Text],
    rowHeaders :: [T.Text],
    dataRows :: [[Double]]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Matrix

generateHistogramData :: (MonadUnliftIO m) => [CategorizedTransaction] -> m Matrix
generateHistogramData transactions = do
  sourceMap <- fetchSourceMap
  let grouped = groupBySourceAndMonth sourceMap transactions
  let allSourceNames = extractAllSourceNames grouped
  let matrix = buildMatrix allSourceNames grouped
  return matrix

groupBySourceAndMonth ::
  Map (Key TransactionSource) TransactionSource ->
  [CategorizedTransaction] ->
  Map T.Text (Map T.Text Double)
groupBySourceAndMonth sourceMap txns =
  Map.fromListWith
    (Map.unionWith (+))
    [ ( formatMonthYear $ transactionDateOfTransaction $ transaction txn,
        Map.singleton (resolveSourceName sourceMap txn) (transactionAmount $ transaction txn)
      )
      | txn <- txns
    ]

resolveSourceName ::
  Map (Key TransactionSource) TransactionSource ->
  CategorizedTransaction ->
  T.Text
resolveSourceName sourceMap txn =
  let sourceId = categorySourceId $ category txn
   in Map.findWithDefault "Unknown" sourceId (Map.map transactionSourceName sourceMap)

extractAllSourceNames :: Map T.Text (Map T.Text Double) -> [T.Text]
extractAllSourceNames groupedData =
  Set.toList $ Set.unions (Prelude.map Map.keysSet $ Map.elems groupedData)

buildMatrix ::
  [T.Text] -> -- List of all source names
  Map T.Text (Map T.Text Double) -> -- Grouped data by month and source
  Matrix
buildMatrix allSourceNames groupedData =
  let columnHeaders = "Month" : allSourceNames
      rowHeaders = Map.keys groupedData
      dataRows = Prelude.map (buildRow allSourceNames) (Map.toList groupedData)
   in Matrix columnHeaders rowHeaders dataRows

buildRow ::
  [T.Text] -> -- List of all source names
  (T.Text, Map T.Text Double) -> -- A single month's data
  [Double]
buildRow allSourceNames (_, sources) =
  Prelude.map (\source -> Map.findWithDefault 0 source sources) allSourceNames

formatMonthYear :: UTCTime -> T.Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%m/%Y" utcTime)

main :: IO ()
main = do
  activeJobs <- newIORef 0

  ConnectionPool.initializeDatabase
  Database.seedDatabase
  migratePostgres
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
      sourceIdText <- Web.Scotty.formParam "transactionSourceId"
      let sourceId = toSqlKey $ read sourceIdText

      liftIO $ persistUploadConfiguration startKeyword endKeyword sourceId filenamePattern

      redirect "/"

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
              maybeConfig <- liftIO $ getUploadConfiguration originalName

              liftIO $ print $ show maybeConfig

              case maybeConfig of
                Just config -> do
                  newPdfId <- liftIO $ insertPdfRecord originalName rawText "TODO"

                  liftIO $ do
                    modifyIORef activeJobs (+ 1)
                    _ <- Control.Concurrent.Async.async $ do
                      processPdfFile newPdfId config
                      modifyIORef activeJobs (subtract 1)
                    return ()

                  redirect "/"
                Nothing -> do
                  newPdfId <- liftIO $ insertPdfRecord originalName rawText "TODO"
                  redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show $ fromSqlKey newPdfId))

    get "/adjust-transactions/:pdfId" $ do
      pdfIdText <- Web.Scotty.param "pdfId"
      liftIO $ print "here"

      let pdfId = toSqlKey (read $ T.unpack pdfIdText) :: Key UploadedPdf

      liftIO $ print "there"
      transactionSources <- liftIO getAllTransactionSources

      liftIO $ print "there2"
      uploadedPdf <- liftIO $ fetchPdfRecord pdfId
      liftIO $ print "there3"

      let segments = T.splitOn "\n" (uploadedPdfRawContent uploadedPdf)

      Web.html $ renderSliderPage pdfId (uploadedPdfFilename uploadedPdf) segments transactionSources

    get "/transactions" $ do
      filenames <- liftIO getAllFilenames
      Web.html $ renderAllFilesPage filenames

    get "/transactions/:fileid" $ do
      fileIdText <- Web.Scotty.pathParam "fileid"

      let fileId = toSqlKey (read $ T.unpack fileIdText) :: Key UploadedPdf
      uploadedFile <- fetchPdfRecord fileId
      transactions <- liftIO $ getTransactionsByFileId fileId
      Web.html $ renderTransactionsPage (uploadedPdfFilename uploadedFile) transactions

    post "/update-category" $ do
      tId <- Web.Scotty.formParam "transactionId"
      newCat <- Web.Scotty.formParam "newCategory"
      let newCatId = toSqlKey (read $ T.unpack newCat) :: Key Category
      fileArg <- Web.Scotty.formParam "filename"
      liftIO $ updateTransactionCategory (read $ T.unpack tId) newCatId
      redirect $ TL.fromStrict ("/transactions/" <> fileArg)

    post "/update-sankey-config" $ do
      configName <- Web.Scotty.formParam "configName"

      allParams <- Web.Scotty.formParams

      let inputSourceIds = [toSqlKey (read $ T.unpack value) | (key, value) <- allParams, key == "inputSourceId[]"]
          inputCategoryIds = [toSqlKey (read $ T.unpack value) | (key, value) <- allParams, key == "inputCategoryId[]"]

      linkageSourceId <- Web.Scotty.formParam "linkageSourceId"
      linkageCategoryId <- Web.Scotty.formParam "linkageCategoryId"
      linkageTargetId <- Web.Scotty.formParam "linkageTargetId"

      inputTransactionSources <- liftIO $ mapM getTransactionSource inputSourceIds
      inputCategories <- liftIO $ mapM getCategory inputCategoryIds

      linkageSource <- liftIO $ getTransactionSource (toSqlKey (read linkageSourceId))
      (linkageCategory, _) <- liftIO $ getCategory (toSqlKey (read linkageCategoryId))
      linkageTarget <- liftIO $ getTransactionSource (toSqlKey (read linkageTargetId))

      -- Assuming rawInputs is created earlier, e.g., by zipping or combining sources and categories
      let rawInputs = zip inputTransactionSources inputCategories
          inputs = Prelude.map (\(source, (category, _)) -> (source, category)) rawInputs
          linkages = (linkageSource, linkageCategory, linkageTarget)
          mapKeyFunction entity = transactionSourceName (entityVal entity)
          newConfig =
            FullSankeyConfig
              { configName = configName,
                inputs = inputs,
                linkages = linkages,
                mapKeyFunction = mapKeyFunction
              }

      liftIO $ saveSankeyConfig newConfig
      redirect "/"

    -- post "/add-upload-config" $ addUploadConfig dbPath
    -- post "/delete-upload-config/:id" $ deleteUploadConfig dbPath
    post "/edit-upload-config/:id" $ do
      -- Extract the ID parameter from the route
      idParam <- Web.Scotty.param "id" :: ActionM Text
      let uploadConfigId = toSqlKey (read $ T.unpack idParam) :: Key UploadConfiguration

      -- Extract form or JSON parameters
      startKeyword <- Web.Scotty.formParam "startKeyword"
      endKeyword <- Web.Scotty.formParam "endKeyword"
      filenameRegex <- Web.Scotty.formParam "filenameRegex"

      -- Call the editUploadConfiguration function
      liftIO $ editUploadConfiguration uploadConfigId startKeyword endKeyword filenameRegex

      -- Respond to the client
      redirect "/configuration"

    post "/add-upload-config" $ do
      filenameRegex <- formParam "filenameRegex" :: ActionM Text
      startKeyword <- formParam "startKeyword" :: ActionM Text
      endKeyword <- formParam "endKeyword" :: ActionM Text
      transactionSourceId <- formParam "transactionSourceId" :: ActionM Text

      let txnSourceId = toSqlKey (read $ T.unpack transactionSourceId) :: Key TransactionSource

      liftIO $ persistUploadConfiguration startKeyword endKeyword txnSourceId filenameRegex

      redirect "/configuration"

    post "/delete-upload-config/:id" $ do
      -- Extract the ID parameter from the route
      idParam <- Web.Scotty.param "id" :: ActionM Text
      let uploadConfigId = toSqlKey (read $ T.unpack idParam) :: Key UploadConfiguration

      -- Call the deleteUploadConfiguration function
      liftIO $ deleteUploadConfiguration uploadConfigId

    -- Respond with a success message

    post "/delete-processed-file" $ do
      filename <- Web.Scotty.formParam "filename" :: Web.Scotty.ActionM Int
      transactionSourceId <- Web.Scotty.formParam "transactionSourceId" :: Web.Scotty.ActionM Int
      -- TODO
      -- liftIO $ deleteProcessedFile dbPath transactionSourceId filename
      Web.Scotty.redirect "/manage-processed-files"

    get "/configuration" $ do
      uploaderConfigs <- liftIO getAllUploadConfigs
      transactionSources <- liftIO getAllTransactionSources
      sankeyConfig <- liftIO getFirstSankeyConfig
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      Web.Scotty.html $ renderConfigurationPage sankeyConfig categoriesBySource uploaderConfigs transactionSources

    post "/add-transaction-source" $ do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ addTransactionSource newSource
      Web.Scotty.redirect "/configuration"

    post "/edit-transaction-source/:id" $ do
      sourceIdText <- Web.Scotty.param "id"
      let sourceId = toSqlKey $ read sourceIdText
      sourceName <- Web.Scotty.formParam "sourceName" :: Web.Scotty.ActionM Text
      liftIO $ updateTransactionSource sourceId sourceName
      Web.Scotty.redirect "/configuration"

    post "/add-category/:sourceId" $ do
      sourceIdText <- Web.Scotty.param "sourceId"
      let sourceId = toSqlKey $ read sourceIdText
      newCategory <- Web.Scotty.formParam "newCategory" :: Web.Scotty.ActionM Text
      liftIO $ addCategory newCategory sourceId
      Web.Scotty.redirect "/configuration"

    post "/edit-category/:id" $ do
      catIdText <- Web.Scotty.param "id"
      let catId = toSqlKey $ read catIdText
      catName <- Web.Scotty.formParam "categoryName"
      liftIO $ updateCategory catId catName
      Web.Scotty.redirect "/configuration"

    get "/api/sankey-data" $ do
      categorizedTransactions <- liftIO getAllTransactions
      gbs <- groupTransactionsBySource categorizedTransactions

      sankeyConfig <- liftIO getFirstSankeyConfig
      let sankeyData = case sankeyConfig of
            Just config -> Just (generateSankeyData gbs config)
            Nothing -> Nothing

      json sankeyData

    get "/api/histogram-data" $ do
      -- Fetch or compute the histogram data
      transactions <- liftIO getAllTransactions
      histogramData <- generateHistogramData transactions
      json histogramData
