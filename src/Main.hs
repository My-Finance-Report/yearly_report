{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
import Categorizer
import ConnectionPool
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Database.Category
  ( addCategory,
    getCategoriesBySource,
    getCategory,
    updateCategory,
  )
import Database.Database
  ( fetchPdfRecord,
    fetchSourceMap,
    getAllFilenames,
    insertPdfRecord,
    seedDatabase,
  )
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql hiding (get)
import Database.TransactionSource
import Database.UploadConfiguration
import Database.Transaction
import Database.Configurations
import GHC.Generics (Generic)
import HtmlGenerators.AllFilesPage
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.HomePage
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.RefineSelectionPage
import Models
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers
import Sankey
import System.FilePath ((</>))
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


extractBearerToken :: TL.Text -> Maybe TL.Text
extractBearerToken header =
  let prefix = "Bearer "
   in if prefix `TL.isPrefixOf` header
        then Just $ TL.drop (TL.length prefix) header
        else Nothing

extractSessionCookie :: TL.Text -> Maybe TL.Text
extractSessionCookie cookies =
  let sessionPrefix = "session="
   in listToMaybe [TL.drop (TL.length sessionPrefix) cookie | cookie <- TL.splitOn "; " cookies, sessionPrefix `TL.isPrefixOf` cookie]

getTokenFromRequest :: ActionM (Maybe TL.Text)
getTokenFromRequest = do
  mCookie <- header "Cookie"
  case mCookie >>= extractSessionCookie of
    Just token -> return $ Just token
    Nothing -> do
      mAuthHeader <- header "Authorization"
      return $ mAuthHeader >>= extractBearerToken

requireUser :: ConnectionPool -> (Entity User -> ActionM ()) -> ActionM ()
requireUser pool action = do
  mToken <- getTokenFromRequest
  case mToken of
    Nothing -> Web.Scotty.redirect "/login" -- Redirect if no token
    Just token -> do
      mUser <- liftIO $ validateSession pool $ TL.toStrict token
      case mUser of
        Nothing -> Web.Scotty.redirect "/login" -- Redirect if token invalid
        Just user -> action user -- Pass the user entity to the action

getCurrentUser :: ConnectionPool -> ActionM (Maybe (Entity User))
getCurrentUser pool = do
  mToken <- getTokenFromRequest
  case mToken of
    Nothing -> return Nothing
    Just token -> liftIO $ validateSession pool $ TL.toStrict token

main :: IO ()
main = do
  activeJobs <- newIORef 0
  ConnectionPool.initializePool
  pool <- getConnectionPool
  migratePostgres
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/login" $ do
      token <- getTokenFromRequest

      case token of
        Just _ -> Web.redirect "/"
        Nothing -> Web.html $ renderPage Nothing "Login" $ renderLoginPage Nothing

    post "/login" $ do
      email <- Web.Scotty.formParam "email" :: Web.Scotty.ActionM Text
      password <- Web.Scotty.formParam "password" :: Web.Scotty.ActionM Text
      maybeUser <- liftIO $ validateLogin pool email password
      case maybeUser of
        Nothing -> Web.html $ renderPage Nothing "Login" $ renderLoginPage (Just "Invalid email or password")
        Just user -> do
          token <- liftIO $ createSession pool (entityKey user)
          Web.setHeader "Set-Cookie" $ TL.fromStrict $ "session=" <> token <> "; Path=/; HttpOnly"
          Web.redirect "/"

    post "/register" $ do
      email <- Web.Scotty.formParam "email" :: Web.Scotty.ActionM Text
      password <- Web.Scotty.formParam "password" :: Web.Scotty.ActionM Text
      confirmPassword <- Web.Scotty.formParam "confirm-password" :: Web.Scotty.ActionM Text

      if password /= confirmPassword
        then Web.html $ renderPage Nothing "Login" $ renderLoginPage (Just "Passwords do not match")
        else do
          result <- liftIO $ createUser pool email password
          case result of
            Left err -> Web.html $ renderPage Nothing "Login" $ renderLoginPage (Just err)
            Right user -> do
              liftIO $ Database.Database.seedDatabase user
              token <- liftIO $ createSession pool (entityKey user)
              Web.setHeader "Set-Cookie" $ TL.fromStrict $ "session=" <> token <> "; Path=/; HttpOnly"
              Web.redirect "/"

    get "/logout" $ do
      mToken <- getTokenFromRequest

      case mToken of
        Nothing -> Web.redirect "/login"
        Just token -> do
          pool <- liftIO getConnectionPool
          liftIO $ deleteSession pool $ TL.toStrict token
          Web.setHeader "Set-Cookie" "session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
          Web.redirect "/login"

    get "/" $ requireUser pool $ \user -> do
      activeJobs <- liftIO $ readIORef activeJobs
      let banner = if activeJobs > 0 then Just "Job Running" else Nothing
      liftIO $ print banner
      content <- liftIO $ renderHomePage user banner
      Web.html $ renderPage (Just user) "Financal Summary" content

    post "/setup-upload" $ requireUser pool $ \user -> do
      startKeyword <- Web.Scotty.formParam "startKeyword" :: ActionM T.Text
      endKeyword <- Web.Scotty.formParam "endKeyword" :: ActionM T.Text
      filenamePattern <- Web.Scotty.formParam "filenamePattern" :: ActionM T.Text
      sourceIdText <- Web.Scotty.formParam "transactionSourceId"
      let sourceId = toSqlKey $ read sourceIdText

      liftIO $ addUploadConfiguration user startKeyword endKeyword sourceId filenamePattern

      redirect "/"

    post "/upload" $ requireUser pool $ \user -> do
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
              maybeConfig <- liftIO $ getUploadConfiguration user originalName

              liftIO $ print $ show maybeConfig

              case maybeConfig of
                Just config -> do
                  newPdfId <- liftIO $ insertPdfRecord originalName rawText "TODO"

                  liftIO $ do
                    modifyIORef activeJobs (+ 1)
                    _ <- Control.Concurrent.Async.async $ do
                      processPdfFile user newPdfId config
                      modifyIORef activeJobs (subtract 1)
                    return ()

                  redirect "/"
                Nothing -> do
                  newPdfId <- liftIO $ insertPdfRecord originalName rawText "TODO"
                  redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show $ fromSqlKey newPdfId))

    get "/adjust-transactions/:pdfId" $ requireUser pool $ \user -> do
      pdfIdText <- Web.Scotty.pathParam "pdfId"

      let pdfId = toSqlKey (read $ T.unpack pdfIdText) :: Key UploadedPdf
      transactionSources <- liftIO $ getAllTransactionSources user
      uploadedPdf <- liftIO $ fetchPdfRecord pdfId
      let segments = T.splitOn "\n" (uploadedPdfRawContent uploadedPdf)

      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderSliderPage pdfId (uploadedPdfFilename uploadedPdf) segments transactionSources

    get "/transactions" $ requireUser pool $ \user -> do
      filenames <- liftIO getAllFilenames
      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderAllFilesPage filenames

    get "/transactions/:fileid" $ requireUser pool $ \user -> do
      fileIdText <- Web.Scotty.pathParam "fileid"

      let fileId = toSqlKey (read $ T.unpack fileIdText) :: Key UploadedPdf
      uploadedFile <- fetchPdfRecord fileId
      transactions <- liftIO $ getTransactionsByFileId user fileId
      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderTransactionsPage (uploadedPdfFilename uploadedFile) transactions

    post "/update-category" $ requireUser pool $ \user -> do
      tId <- Web.Scotty.formParam "transactionId"
      newCat <- Web.Scotty.formParam "newCategory"
      let newCatId = toSqlKey (read $ T.unpack newCat) :: Key Category
      fileArg <- Web.Scotty.formParam "filename"
      liftIO $ updateTransactionCategory user (read $ T.unpack tId) newCatId
      redirect $ TL.fromStrict ("/transactions/" <> fileArg)

    post "/update-sankey-config" $ requireUser pool $ \user -> do
      configName <- Web.Scotty.formParam "configName"

      allParams <- Web.Scotty.formParams

      let inputSourceIds = [toSqlKey (read $ T.unpack value) | (key, value) <- allParams, key == "inputSourceId[]"]
          inputCategoryIds = [toSqlKey (read $ T.unpack value) | (key, value) <- allParams, key == "inputCategoryId[]"]

      linkageSourceId <- Web.Scotty.formParam "linkageSourceId"
      linkageCategoryId <- Web.Scotty.formParam "linkageCategoryId"
      linkageTargetId <- Web.Scotty.formParam "linkageTargetId"

      inputTransactionSources <- liftIO $ mapM (getTransactionSource user) inputSourceIds
      inputCategories <- liftIO $ mapM (getCategory user) inputCategoryIds

      linkageSource <- liftIO $ getTransactionSource user (toSqlKey (read linkageSourceId))
      (linkageCategory, _) <- liftIO $ getCategory user (toSqlKey (read linkageCategoryId))
      linkageTarget <- liftIO $ getTransactionSource user (toSqlKey (read linkageTargetId))

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

    get "/configuration" $ requireUser pool $ \user -> do
      uploaderConfigs <- liftIO $ getAllUploadConfigs user
      transactionSources <- liftIO $ getAllTransactionSources user
      sankeyConfig <- liftIO getFirstSankeyConfig
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      Web.Scotty.html $ renderPage (Just user) "Configuration" $ renderConfigurationPage sankeyConfig categoriesBySource uploaderConfigs transactionSources

    post "/add-transaction-source" $ requireUser pool $ \user -> do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ addTransactionSource user newSource
      Web.Scotty.redirect "/configuration"

    post "/edit-transaction-source/:id" $ requireUser pool $ \user -> do
      sourceIdText <- Web.Scotty.pathParam "id"
      let sourceId = toSqlKey $ read sourceIdText
      sourceName <- Web.Scotty.formParam "sourceName" :: Web.Scotty.ActionM Text
      liftIO $ updateTransactionSource user sourceId sourceName
      Web.Scotty.redirect "/configuration"

    post "/add-category/:sourceId" $ requireUser pool $ \user -> do
      sourceIdText <- Web.Scotty.pathParam "sourceId"
      let sourceId = toSqlKey $ read sourceIdText
      newCategory <- Web.Scotty.formParam "newCategory" :: Web.Scotty.ActionM Text
      liftIO $ addCategory user newCategory sourceId
      Web.Scotty.redirect "/configuration"

    post "/edit-category/:id" $ requireUser pool $ \user -> do
      catIdText <- Web.Scotty.pathParam "id"
      let catId = toSqlKey $ read catIdText
      catName <- Web.Scotty.formParam "categoryName"
      liftIO $ updateCategory user catId catName
      Web.Scotty.redirect "/configuration"

    get "/api/sankey-data" $ requireUser pool $ \user -> do
      categorizedTransactions <- liftIO $ getAllTransactions user
      gbs <- groupTransactionsBySource user categorizedTransactions

      sankeyConfig <- liftIO getFirstSankeyConfig
      let sankeyData = case sankeyConfig of
            Just config -> Just (generateSankeyData gbs config)
            Nothing -> Nothing

      json sankeyData

    get "/api/histogram-data" $ requireUser pool $ \user -> do
      -- Fetch or compute the histogram data
      transactions <- liftIO $ getAllTransactions user
      histogramData <- generateHistogramData transactions
      json histogramData
