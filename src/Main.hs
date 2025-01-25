{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HList (newIORef)
import Database.ConnectionPool
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Routes.Api.Visualization.RegisterVisualization (registerVisualizationRoutes)
import Routes.Configuration.RegisterConfiguration (registerConfigurationRoutes)
import Routes.Crud.Category.RegisterCategory (registerCategoryRoutes)
import Routes.Crud.File.RegisterFile (registerFileRoutes)
import Routes.Crud.Sankey.RegisterSankey (registerSankeyRoutes)
import Routes.Crud.Transaction.RegisterTransaction (registerTransactionRoutes)
import Routes.Crud.TransactionSource.RegisterTransactionSource (registerTransactionSourceRoutes)
import Routes.Demo.RegisterDemo (registerDemoRoutes)
import Routes.Login.RegisterLogin (registerLoginRoutes)
import Routes.Misc.RegisterMisc (registerMiscRoutes)
import Routes.Onboarding.RegisterOnboarding
  ( registerOnboardingRoutes,
  )
import Routes.Upload.RegisterUpload (registerUploadRoutes)
import System.Environment (lookupEnv)
import Web.Scotty (middleware, scotty)
import qualified Web.Scotty as Web

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
  openAiKey <- liftIO $ getRequiredEnv "DATABASE_URL"
  initializePool
  pool <- getConnectionPool
  migratePostgres
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    registerOnboardingRoutes pool
    registerLoginRoutes pool
    registerMiscRoutes pool activeJobs
    registerVisualizationRoutes pool
    registerDemoRoutes pool
    registerSankeyRoutes pool
    registerConfigurationRoutes pool
    registerUploadRoutes pool activeJobs
    registerCategoryRoutes pool
    registerTransactionRoutes pool
    registerTransactionSourceRoutes pool
    registerFileRoutes pool activeJobs
