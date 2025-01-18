{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
import Categorizer
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, throwIO, try)
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
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, parseTimeM, toGregorian)
import Database.Category
  ( addCategory,
    getCategoriesBySource,
    getCategory,
    removeCategory,
    updateCategory,
  )
import Database.Configurations
import Database.ConnectionPool
import Database.Database (getDemoUser, updateUserOnboardingStep)
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
import ExampleFileParser
import GHC.Generics (Generic)
import HtmlGenerators.AllFilesPage
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.LandingPage
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.OnboardingFour
import HtmlGenerators.OnboardingOne
import HtmlGenerators.OnboardingThree
import HtmlGenerators.OnboardingTwo
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers
import Sankey
import SankeyConfiguration
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
    Nothing -> Web.Scotty.redirect "/login"
    Just token -> do
      mUser <- liftIO $ validateSession pool $ TL.toStrict token
      case mUser of
        Nothing -> Web.Scotty.redirect "/login"
        Just user -> action user

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

    get "/test" $ do
      user <- getDemoUser
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories
      liftIO $ generateSankeyConfig categoriesBySource
      Web.html "ok"

    get "/" $ do
      pool <- liftIO getConnectionPool
      user <- getCurrentUser pool
      case user of
        Just user -> Web.redirect "/dashboard"
        Nothing -> Web.html $ renderPage user "My Financial Report" renderLandingPage

    get "/onboarding" $ requireUser pool $ \user -> do
      let currentStep = userOnboardingStep $ entityVal user
      case currentStep of
        Just 0 -> redirect "/onboarding/step-1"
        Just 1 -> redirect "/onboarding/step-2"
        Just 2 -> redirect "/onboarding/step-3"
        _ -> redirect "/dashboard"

    get "/onboarding/step-1" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      let content = renderOnboardingOne user transactionSources True
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    get "/add-account/step-1" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      let content = renderOnboardingOne user transactionSources False
      Web.Scotty.html $ renderPage (Just user) "Add Account" content

    post "/onboarding/step-1" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user (Just 1)
      redirect "/onboarding/step-2"

    get "/onboarding/step-2" $ requireUser pool $ \user -> do
      liftIO $ print "we are in step 2"
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      let content = renderOnboardingTwo user categoriesBySource True
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    get "/add-account/step-2" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      let content = renderOnboardingTwo user categoriesBySource False
      Web.Scotty.html $ renderPage (Just user) "Add Account" content

    post "/onboarding/step-2" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user (Just 2)
      redirect "/onboarding/step-2"

    get "/onboarding/step-3" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      uploadConfigs <- liftIO $ getAllUploadConfigs user
      let content = renderOnboardingThree user transactionSources (map entityVal uploadConfigs) True
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    get "/add-account/step-3" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      uploadConfigs <- liftIO $ getAllUploadConfigs user
      let content = renderOnboardingThree user transactionSources (map entityVal uploadConfigs) False
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    post "/onboarding/step-3" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user Nothing
      redirect "/onboarding/step-3"

    get "/onboarding/step-4" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      let content = renderOnboardingFour user categoriesBySource
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    post "/onboarding/step-4" $ requireUser pool $ \user -> do
      liftIO $ updateUserOnboardingStep user Nothing
      redirect "/dashboard"

    get "/demo-account" $ do
      demoUser <- getDemoUser
      content <- liftIO $ renderHomePage demoUser (Just makeDemoBanner)
      Web.Scotty.html $ renderPage (Just demoUser) "Financial Summary" content

    get "/dashboard" $ requireUser pool $ \user -> do
      let onboardingStep = userOnboardingStep $ entityVal user
      case onboardingStep of
        Just _ -> Web.Scotty.redirect "/onboarding"
        Nothing -> do
          activeJobs <- liftIO $ readIORef activeJobs
          let banner = if activeJobs > 0 then Just $ makeSimpleBanner "Processing transactions, check back soon!" else Nothing
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
              maybeConfig <- liftIO $ getUploadConfiguration user originalName rawText

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
                  content <- liftIO $ renderHomePage user (Just $ makeSimpleBanner "Looks like we don't know how to process that one. Add a new Account if this is a new transaction")
                  Web.html $ renderPage (Just user) "Dashboard" content

    get "/help" $ do
      pool <- liftIO getConnectionPool
      user <- getCurrentUser pool
      Web.html $ renderPage user "Help Me" $ renderSupportPage

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
      allParams <- Web.Scotty.formParams

      let parseComposite keyValue =
            case T.splitOn "-" keyValue of
              [srcId, catId] -> Just (toSqlKey (read $ T.unpack srcId), toSqlKey (read $ T.unpack catId))
              _ -> Nothing

          inputPairs = [pair | (key, value) <- allParams, key == "inputSourceCategory[]", Just pair <- [parseComposite value]]

      linkageSourceCategory <- Web.Scotty.formParam "linkageSourceCategory"
      linkageTargetId <- Web.Scotty.formParam "linkageTargetId"

      let (linkageSourceId, linkageCategoryId) = fromMaybe (error "Invalid linkage format") (parseComposite linkageSourceCategory)

      -- Fetch database entities
      inputTransactionSources <- liftIO $ mapM (getTransactionSource user . fst) inputPairs
      inputCategories <- liftIO $ mapM (getCategory user . snd) inputPairs
      linkageSource <- liftIO $ getTransactionSource user linkageSourceId
      (linkageCategory, _) <- liftIO $ getCategory user linkageCategoryId
      linkageTarget <- liftIO $ getTransactionSource user (toSqlKey (read linkageTargetId))

      let rawInputs = zip inputTransactionSources inputCategories
          inputs = Prelude.map (\(source, (category, _)) -> (source, category)) rawInputs
          linkages = (linkageSource, linkageCategory, linkageTarget)
          newConfig = FullSankeyConfig {inputs = inputs, linkages = linkages}

      liftIO $ saveSankeyConfig user newConfig
      redirect "/dashboard"

    get "/new-configuration" $ requireUser pool $ \user -> do
      uploaderConfigs <- liftIO $ getAllUploadConfigs user
      transactionSources <- liftIO $ getAllTransactionSources user
      sankeyConfig <- liftIO $ getFirstSankeyConfig user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      Web.Scotty.html $ renderPage (Just user) "Configuration" $ renderConfigurationPageNew sankeyConfig categoriesBySource uploaderConfigs transactionSources

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

    post "/remove-transaction-source" $ requireUser pool $ \user -> do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ removeTransactionSource user newSource

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

    post "/remove-category/:catId" $ requireUser pool $ \user -> do
      catIdText <- Web.Scotty.pathParam "catId"
      let catId = toSqlKey $ read catIdText
      liftIO $ removeCategory user catId

      referer <- Web.Scotty.header "Referer"
      let redirectTo = fromMaybe "/dashboard" referer

      Web.Scotty.redirect redirectTo

    post "/remove-transaction/:tId" $ requireUser pool $ \user -> do
      tIdText <- Web.Scotty.pathParam "tId"
      let tId = toSqlKey $ read tIdText
      liftIO $ removeTransaction user tId

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
      transactions <- liftIO $ getAllTransactions user
      histogramData <- generateHistogramData user transactions
      json histogramData

    get "/demo/api/sankey-data" $ do
      user <- getDemoUser
      categorizedTransactions <- liftIO $ getAllTransactions user
      gbs <- groupTransactionsBySource user categorizedTransactions

      sankeyConfig <- liftIO $ getFirstSankeyConfig user
      let sankeyData = case sankeyConfig of
            Just config -> Just (generateSankeyData gbs config)
            Nothing -> Nothing

      json sankeyData

    get "/demo/api/histogram-data" $ do
      user <- getDemoUser
      transactions <- liftIO $ getAllTransactions user
      histogramData <- generateHistogramData user transactions
      json histogramData

    post "/upload-example-file/:sourceId" $ requireUser pool $ \user -> do
      sourceIdText <- Web.Scotty.pathParam "sourceId" :: Web.Scotty.ActionM Text
      let sourceId = toSqlKey (read $ T.unpack sourceIdText) :: Key TransactionSource

      files <- Web.Scotty.files
      case lookup "exampleFile" files of
        Just fileInfo -> do
          let uploadedBytes = fileContent fileInfo
          let originalName = decodeUtf8 $ fileName fileInfo
          let tempFilePath = "/tmp/" <> originalName
          liftIO $ B.writeFile (T.unpack tempFilePath) uploadedBytes

          uploadConfig <- liftIO $ generateUploadConfiguration user sourceId tempFilePath
          case uploadConfig of
            Just config -> addUploadConfigurationObject user config
            Nothing -> liftIO $ putStrLn "Failed to generate UploadConfiguration from the example file."

          referer <- Web.Scotty.header "Referer"
          let redirectTo = fromMaybe "/dashboard" referer
          Web.Scotty.redirect redirectTo
        Nothing -> do
          Web.Scotty.text "No file provided in the request"
