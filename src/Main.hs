{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
import Categorizer
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
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, parseTimeM, toGregorian)
import Database.Category
  ( addCategory,
    getCategoriesBySource,
    getCategory,
    updateCategory,
  )
import Database.Configurations
import Database.ConnectionPool
import Database.Database (updateUserOnboardingStep)
import Database.Files
import Database.Models
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql hiding (get)
import Database.Transaction
import Database.TransactionSource
import Database.UploadConfiguration
import GHC.Generics (Generic)
import HtmlGenerators.AllFilesPage
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.HomePage
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.LandingPage
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.OnboardingOne
import HtmlGenerators.OnboardingTwo
import HtmlGenerators.RefineSelectionPage
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers
import Sankey
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)
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

parseDate :: Text -> Maybe UTCTime
parseDate dateText = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText)

truncateToMonth :: UTCTime -> UTCTime
truncateToMonth utcTime =
  let (year, month, _) = toGregorian (utctDay utcTime)
   in UTCTime (fromGregorian year month 1) 0

generateHistogramData :: (MonadUnliftIO m) => Entity User -> [CategorizedTransaction] -> m Matrix
generateHistogramData user transactions = do
  sourceMap <- fetchSourceMap user
  let grouped = groupBySourceAndMonth sourceMap transactions
  let allSourceNames = extractAllSourceNames grouped
  let matrix = buildMatrix allSourceNames grouped
  return matrix

groupBySourceAndMonth ::
  Map (Key TransactionSource) TransactionSource ->
  [CategorizedTransaction] ->
  Map UTCTime (Map T.Text Double)
groupBySourceAndMonth sourceMap txns =
  Map.fromListWith
    (Map.unionWith (+))
    [ ( truncateToMonth $ transactionDateOfTransaction $ entityVal $ transaction txn,
        Map.singleton (resolveSourceName sourceMap txn) (transactionAmount $ entityVal $ transaction txn)
      )
      | txn <- txns
    ]

resolveSourceName ::
  Map (Key TransactionSource) TransactionSource ->
  CategorizedTransaction ->
  T.Text
resolveSourceName sourceMap txn =
  let sourceId = categorySourceId $ entityVal $ category txn
   in Map.findWithDefault "Unknown" sourceId (Map.map transactionSourceName sourceMap)

extractAllSourceNames :: Map UTCTime (Map T.Text Double) -> [T.Text]
extractAllSourceNames groupedData =
  Set.toList $ Set.unions (Prelude.map Map.keysSet $ Map.elems groupedData)

buildMatrix ::
  [Text] ->
  Map UTCTime (Map T.Text Double) -> -- Grouped data by date and source
  Matrix
buildMatrix allSourceNames groupedData =
  let columnHeaders = "Month" : allSourceNames
      rowHeaders = Prelude.map formatMonthYear (Map.keys groupedData)
      dataRows = Prelude.map (buildRow allSourceNames) (Map.toList groupedData)
   in Matrix columnHeaders rowHeaders dataRows

buildRow ::
  [T.Text] ->
  (UTCTime, Map T.Text Double) ->
  [Double]
buildRow allSourceNames (_, sources) =
  Prelude.map (\source -> Map.findWithDefault 0 source sources) allSourceNames

formatMonthYear :: UTCTime -> T.Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%b %Y" utcTime)

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

getRequiredEnv :: String -> IO String
getRequiredEnv key = do
  maybeValue <- lookupEnv key
  case maybeValue of
    Just value -> return value
    Nothing -> ioError $ userError $ "Environment variable not set: " ++ key

main :: IO ()
main = do
  activeJobs <- newIORef 0
  openAiKey <- liftIO $ getRequiredEnv "OPENAI_API_KEY"
  initializePool
  pool <- getConnectionPool
  migratePostgres
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/login" $ do
      token <- getTokenFromRequest

      case token of
        Just _ -> Web.redirect "/dashboard"
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
          Web.redirect "/dashboard"

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
              token <- liftIO $ createSession pool (entityKey user)
              Web.setHeader "Set-Cookie" $ TL.fromStrict $ "session=" <> token <> "; Path=/; HttpOnly"
              Web.redirect "/dashboard"

    get "/logout" $ do
      mToken <- getTokenFromRequest

      case mToken of
        Nothing -> Web.redirect "/login"
        Just token -> do
          pool <- liftIO getConnectionPool
          liftIO $ deleteSession pool $ TL.toStrict token
          Web.setHeader "Set-Cookie" "session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
          Web.redirect "/login"

    get "/" $ do
      let content = renderLandingPage
      Web.html $ renderPage Nothing "My Financial Report" content

    get "/onboarding" $ requireUser pool $ \user -> do
      let currentStep = userOnboardingStep $ entityVal user
      case currentStep of
        Just 0 -> redirect "/onboarding/step-1"
        Just 1 -> redirect "/onboarding/step-2"
        Just 2 -> redirect "/onboarding/step-3"
        _ -> redirect "/dashboard"

    get "/onboarding/step-1" $ requireUser pool $ \user -> do
      transactionSources<- liftIO $ getAllTransactionSources user
      let content = renderOnboardingOne user transactionSources
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    post "/onboarding/step-1" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user (Just 1)
      redirect "/onboarding/step-2"

    get "/onboarding/step-2" $ requireUser pool $ \user -> do

      transactionSources <- liftIO $ getAllTransactionSources user
      sankeyConfig <- liftIO $ getFirstSankeyConfig user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      let content = renderOnboardingTwo user categoriesBySource 
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content


    post "/onboarding/step-2" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user (Just 2)
      redirect "/onboarding/step-3"

    get "/onboarding/step-3" $ requireUser pool $ \user -> do
      html "Welcome to step 3 of onboarding!"

    post "/onboarding/step-3" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user Nothing
      redirect "/dashboard"

    get "/dashboard" $ requireUser pool $ \user -> do
      let onboardingStep = userOnboardingStep $ entityVal user
      case onboardingStep of
        Just _ -> Web.Scotty.redirect "/onboarding"
        Nothing -> do
          activeJobs <- liftIO $ readIORef activeJobs
          let banner = if activeJobs > 0 then Just "Job Running" else Nothing
          liftIO $ print banner
          content <- liftIO $ renderHomePage user banner
          Web.Scotty.html $ renderPage (Just user) "Financial Summary" content

    post "/setup-upload" $ requireUser pool $ \user -> do
      startKeyword <- Web.Scotty.formParam "startKeyword" :: ActionM T.Text
      endKeyword <- Web.Scotty.formParam "endKeyword" :: ActionM T.Text
      filenamePattern <- Web.Scotty.formParam "filenamePattern" :: ActionM T.Text
      sourceIdText <- Web.Scotty.formParam "transactionSourceId"
      let sourceId = toSqlKey $ read sourceIdText

      liftIO $ addUploadConfiguration user startKeyword endKeyword sourceId filenamePattern

      redirect "/dashboard"

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
                  newPdfId <- liftIO $ addPdfRecord user originalName rawText "TODO"

                  liftIO $ do
                    modifyIORef activeJobs (+ 1)
                    _ <- Control.Concurrent.Async.async $ do
                      processPdfFile user newPdfId config
                      modifyIORef activeJobs (subtract 1)
                    return ()

                  redirect "/dashboard"
                Nothing -> do
                  newPdfId <- liftIO $ addPdfRecord user originalName rawText "TODO"
                  redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show $ fromSqlKey newPdfId))

    get "/adjust-transactions/:pdfId" $ requireUser pool $ \user -> do
      pdfIdText <- Web.Scotty.pathParam "pdfId"

      let pdfId = toSqlKey (read $ T.unpack pdfIdText) :: Key UploadedPdf
      transactionSources <- liftIO $ getAllTransactionSources user
      uploadedPdf <- liftIO $ getPdfRecord user pdfId
      let segments = T.splitOn "\n" (uploadedPdfRawContent $ entityVal uploadedPdf)

      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderSliderPage pdfId (uploadedPdfFilename $ entityVal uploadedPdf) segments transactionSources

    get "/transactions" $ requireUser pool $ \user -> do
      filenames <- liftIO $ getAllFilenames user
      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderAllFilesPage filenames

    get "/transactions/:fileid" $ requireUser pool $ \user -> do
      fileIdText <- Web.Scotty.pathParam "fileid"

      let fileId = toSqlKey (read $ T.unpack fileIdText) :: Key UploadedPdf
      uploadedFile <- getPdfRecord user fileId
      transactions <- liftIO $ getTransactionsByFileId user fileId
      transactionSources <- liftIO $ getAllTransactionSources user
      categoryLookup <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip (Prelude.map entityKey transactionSources) categories

      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderTransactionsPage uploadedFile categoryLookup transactions

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

      liftIO $ saveSankeyConfig user newConfig
      redirect "/dashboard"

    get "/configuration" $ requireUser pool $ \user -> do
      uploaderConfigs <- liftIO $ getAllUploadConfigs user
      transactionSources <- liftIO $ getAllTransactionSources user
      sankeyConfig <- liftIO $ getFirstSankeyConfig user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      Web.Scotty.html $ renderPage (Just user) "Configuration" $ renderConfigurationPage sankeyConfig categoriesBySource uploaderConfigs transactionSources

    post "/add-transaction-source" $ requireUser pool $ \user -> do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ addTransactionSource user newSource

      referer <- Web.Scotty.header "Referer"
      let redirectTo = fromMaybe "/dashboard" referer

      Web.Scotty.redirect redirectTo


    post "/edit-transaction-source/:id" $ requireUser pool $ \user -> do
      sourceIdText <- Web.Scotty.pathParam "id"
      let sourceId = toSqlKey $ read sourceIdText
      sourceName <- Web.Scotty.formParam "sourceName" :: Web.Scotty.ActionM Text
      liftIO $ updateTransactionSource user sourceId sourceName
      Web.Scotty.redirect "/configuration"

    post "/update-transaction/:id" $ requireUser pool $ \user -> do
      txIdText <- Web.Scotty.pathParam "id" :: Web.Scotty.ActionM Int
      fileIdText <- Web.Scotty.formParam "fileId" :: Web.Scotty.ActionM Text
      let txId = toSqlKey (fromIntegral txIdText)

      mDescription <- Web.Scotty.formParam "description" >>= \d -> return $ readMaybe d
      mDate <- Web.Scotty.formParam "transactionDate" >>= \d -> return $ parseDate d
      mAmount <- Web.Scotty.formParam "amount" >>= \a -> return $ readMaybe a
      mCategoryId <- Web.Scotty.formParam "category" >>= \c -> return $ Just (toSqlKey $ read c)
      mKind <-
        Web.Scotty.formParam "kind" >>= \k -> return $ case (k :: Text) of
          "Withdrawal" -> Just Withdrawal
          "Deposit" -> Just Deposit
          _ -> Nothing

      liftIO $ updateTransaction user txId mDescription mDate mAmount mKind mCategoryId

      let redirectUrl = TL.fromStrict $ T.append "/transactions/" fileIdText
      Web.Scotty.redirect redirectUrl

    post "/add-category/:sourceId" $ requireUser pool $ \user -> do
      sourceIdText <- Web.Scotty.pathParam "sourceId"
      let sourceId = toSqlKey $ read sourceIdText
      newCategory <- Web.Scotty.formParam "newCategory" :: Web.Scotty.ActionM Text
      liftIO $ addCategory user newCategory sourceId

      referer <- Web.Scotty.header "Referer"
      let redirectTo = fromMaybe "/dashboard" referer

      Web.Scotty.redirect redirectTo

    post "/edit-category/:id" $ requireUser pool $ \user -> do
      catIdText <- Web.Scotty.pathParam "id"
      let catId = toSqlKey $ read catIdText
      catName <- Web.Scotty.formParam "categoryName"
      liftIO $ updateCategory user catId catName
      Web.Scotty.redirect "/configuration"

    get "/api/sankey-data" $ requireUser pool $ \user -> do
      categorizedTransactions <- liftIO $ getAllTransactions user
      gbs <- groupTransactionsBySource user categorizedTransactions

      sankeyConfig <- liftIO $ getFirstSankeyConfig user
      let sankeyData = case sankeyConfig of
            Just config -> Just (generateSankeyData gbs config)
            Nothing -> Nothing

      json sankeyData

    get "/api/histogram-data" $ requireUser pool $ \user -> do
      -- Fetch or compute the histogram data
      transactions <- liftIO $ getAllTransactions user
      histogramData <- generateHistogramData user transactions
      json histogramData
