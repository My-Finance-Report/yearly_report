{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
import Categorizer
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.List (foldl', nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
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
import ColumnChart 
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
import HtmlGenerators.UploadPage
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
  ( ActionM,
    File,
    files,
    finish,
    formParam,
    formParams,
    get,
    header,
    html,
    json,
    liftIO,
    middleware,
    pathParam,
    post,
    redirect,
    scotty,
    text,
  )
import qualified Web.Scotty as Web


parseDate :: Text -> Maybe UTCTime
parseDate dateText = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText)


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

deleteFileAndTransactions ::
  Entity User ->
  Key ProcessedFile ->
  IORef Int ->
  Web.Scotty.ActionM ()
deleteFileAndTransactions user processedFileId activeJobs = do
  processedFile <- liftIO $ getProcessedFile user processedFileId
  let fileId = entityKey processedFile
  liftIO $ putStrLn "Deleting with a valid config"
  let pdfId = processedFileUploadedPdfId $ entityVal processedFile
  case pdfId of
    Nothing -> redirect "/dashboard"
    Just validPdfId -> do
      liftIO $ removeTransactionByPdfId user validPdfId
      liftIO $ deleteProcessedFile user fileId

reprocessFileUpload ::
  Entity User ->
  Key ProcessedFile ->
  IORef Int ->
  Web.Scotty.ActionM ()
reprocessFileUpload user processedFileId activeJobs = do
  processedFile <- liftIO $ getProcessedFile user processedFileId
  let pdfId = processedFileUploadedPdfId $ entityVal processedFile
  let uploadConfigId = processedFileUploadConfigurationId $ entityVal processedFile

  liftIO $ print (show pdfId <> show uploadConfigId)

  case (pdfId, uploadConfigId) of
    (Nothing, Nothing) -> Web.text "Error: Processed file does not have an associated uploaded PDF or config"
    (Nothing, _) -> Web.text "Error: Processed file does not have an associated uploaded PDF."
    (_, Nothing) -> Web.text "Error: Processed file does not have an associated upload configuration."
    (Just validPdfId, Just validUploadConfigId) -> do
      liftIO $ putStrLn "Reprocessing with a valid config"
      liftIO $ do
        putStrLn $ "Reprocessing PDF ID: " <> show (fromSqlKey validPdfId)
        putStrLn $ "Using upload config ID: " <> show (fromSqlKey validUploadConfigId)

        modifyIORef activeJobs (+ 1)
        _ <- Control.Concurrent.Async.async $ do
          uploadConfig <- getUploadConfigById user validUploadConfigId
          removeTransactionByPdfId user validPdfId
          processPdfFile user validPdfId uploadConfig True
          modifyIORef activeJobs (subtract 1)
          putStrLn $ "Finished reprocessing PDF ID: " <> show (fromSqlKey validPdfId)
          return ()
        return ()

      -- Ensure response is sent after the async job starts
      Web.text "Reprocessing started successfully!"

processFileUpload user pdfId config activeJobs = do
  liftIO $ do
    modifyIORef activeJobs (+ 1)
    _ <- Control.Concurrent.Async.async $ do
      processPdfFile user pdfId config False
      modifyIORef activeJobs (subtract 1)
    return ()
  return ()

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
      liftIO $ updateUserOnboardingStep user Nothing
      redirect "/onboarding/step-2"

    get "/add-account/step-3" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      uploadConfigs <- liftIO $ getAllUploadConfigs user
      let content = renderOnboardingThree user transactionSources (map entityVal uploadConfigs) False
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    get "/onboarding/step-4" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      let content = renderOnboardingFour user categoriesBySource
      Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

    post "/onboarding/finalize" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      liftIO $ do
        void $ async $ do
          config <- generateSankeyConfig user categoriesBySource
          case config of
            Just con -> do
              saveSankeyConfig user con
              -- since we dont actually need the result
              return ()
            Nothing -> putStrLn "Error: Failed to generate Sankey configuration."
      liftIO $ updateUserOnboardingStep user Nothing
      Web.Scotty.redirect "/dashboard"

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

    get "/manage-accounts" $ requireUser pool $ \user -> do
      Web.Scotty.html $ renderPage (Just user) "Manage Accounts" "this page is not ready yet :) "

    get "/select-account" $ requireUser pool $ \user -> do
      pdfIdsParam <- Web.queryParam "pdfIds"

      -- Fetch pending PDF records from the database
      let pdfIds = map (toSqlKey . read . T.unpack) pdfIdsParam :: [Key UploadedPdf]
      fileRecords <- liftIO $ getPdfRecords user pdfIds

      liftIO $ print pdfIds

      transactionSources <- liftIO $ getAllTransactionSources user

      let txnLookup = Map.fromList [(entityKey txn, txn) | txn <- transactionSources]

      liftIO $ print txnLookup

      pdfsWithSources <- forM fileRecords $ \record -> do
        maybeConfig <- liftIO $ getUploadConfigurationFromPdf user (entityKey record)
        let maybeTxnSource = maybeConfig >>= \(Entity _ config) -> Map.lookup (uploadConfigurationTransactionSourceId config) txnLookup
        return (record, maybeTxnSource)

      transactionSources <- liftIO $ getAllTransactionSources user

      liftIO $ print "making it to here"

      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderSelectAccountPage pdfsWithSources transactionSources

    get "/upload" $ requireUser pool $ \user -> do
      let content = renderUploadPage user
      Web.html $ renderPage (Just user) "Upload Page" content

    post "/upload" $ requireUser pool $ \user -> do
      allFiles <- Web.Scotty.files
      let uploadedFiles = Prelude.filter (\(k, _) -> k == "pdfFiles") allFiles
      case uploadedFiles of
        [] -> Web.text "No files were uploaded!"
        _ -> do
          fileConfigs <- forM uploadedFiles $ \(_, fileInfo) -> do
            let originalName = decodeUtf8 $ fileName fileInfo
            let tempFilePath = "/tmp/" <> originalName

            liftIO $ B.writeFile (T.unpack tempFilePath) (fileContent fileInfo)

            extractedTextOrError <-
              liftIO $ try (extractTextFromPdf (T.unpack tempFilePath)) :: ActionM (Either SomeException Text)

            case extractedTextOrError of
              Left err -> return (fileInfo, Nothing, Nothing)
              Right extractedText -> do
                maybeConfig <- liftIO $ getUploadConfiguration user originalName extractedText
                return (fileInfo, maybeConfig, Just extractedText)

          pdfIds <- forM fileConfigs $ \(fileInfo, maybeConfig, maybeExtractedText) -> do
            let originalName = decodeUtf8 $ fileName fileInfo
            pdfId <- liftIO $ addPdfRecord user originalName (fromMaybe "" maybeExtractedText) "pending"
            return (pdfId, maybeConfig)

          let (missingConfigs, validConfigs) = partition (isNothing . snd) pdfIds

          if null missingConfigs
            then do
              forM_ validConfigs $ \(pdfId, Just config) -> do
                processFileUpload user pdfId config activeJobs
              redirect "/dashboard"
            else do
              redirect $ "/select-account?pdfIds=" <> TL.intercalate "," (map (TL.pack . show . fromSqlKey . fst) pdfIds)

    get "/help" $ do
      pool <- liftIO getConnectionPool
      user <- getCurrentUser pool
      Web.html $ renderPage user "Help Me" renderSupportPage

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
          newConfig = FullSankeyConfig {inputs = inputs, linkages = [linkages]} -- TODO this needs to handle multiple
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

    post "/reprocess-file/:fId" $ requireUser pool $ \user -> do
      fIdText <- Web.Scotty.pathParam "fId"

      let processedFileId = toSqlKey $ read fIdText

      liftIO $ print "reprocessing"
      reprocessFileUpload user processedFileId activeJobs

      Web.Scotty.redirect "/dashboard"

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
      histogramData <- generateColChartData user transactions
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
      histogramData <- generateColChartData user transactions
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

          liftIO $ do
            uploadConfig <- generateUploadConfiguration user sourceId tempFilePath
            case uploadConfig of
              Just config -> addUploadConfigurationObject user config
              Nothing -> putStrLn "Failed to generate UploadConfiguration from the example file."

          referer <- Web.Scotty.header "Referer"
          let redirectTo = fromMaybe "/dashboard" referer
          Web.Scotty.redirect redirectTo
        Nothing -> Web.Scotty.text "No file provided in the request"

    post "/assign-transaction-source" $ requireUser pool $ \user -> do
      params <- Web.formParams

      let selections =
            [ (toSqlKey (read $ T.unpack (T.drop 7 key)) :: Key UploadedPdf, toSqlKey (read $ T.unpack val) :: Key TransactionSource)
              | (key, val) <- params,
                "source-" `T.isPrefixOf` key
            ]
      liftIO $ print selections

      -- Process each selected transaction source
      forM_ selections $ \(pdfId, sourceId) -> do
        pdfRecord <- liftIO $ getPdfRecord user pdfId

        -- Ensure we have a valid file path to use for generating config
        let tempFilePath = "/tmp/" <> uploadedPdfFilename (entityVal pdfRecord)

        -- Generate and store the UploadConfiguration
        uploadConfig <- liftIO $ generateUploadConfiguration user sourceId tempFilePath
        case uploadConfig of
          Just config -> liftIO $ addUploadConfigurationObject user config
          Nothing -> liftIO $ putStrLn "Failed to generate UploadConfiguration."

      liftIO $ print "created the upload config"

      -- Reprocess all uploaded files
      forM_ selections $ \(pdfId, sourceId) -> do
        pdfRecord <- liftIO $ getPdfRecord user pdfId
        let extractedText = uploadedPdfRawContent (entityVal pdfRecord)
        maybeConfig <- liftIO $ getUploadConfigurationFromPdf user pdfId

        case maybeConfig of
          Just config -> processFileUpload user pdfId config activeJobs
          Nothing -> liftIO $ putStrLn "Failed to retrieve configuration for reprocessing."

      redirect "/dashboard"
