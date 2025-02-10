{-# LANGUAGE OverloadedStrings #-}

module Routes.Crud.File.RegisterFile (registerFileRoutes) where

import Auth (requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.HList (IORef, modifyIORef)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (addCategory, getCategoriesBySource, getCategory, removeCategory, updateCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Models (Category (Category), UploadedPdf, User (userOnboardingStep))
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, fromSqlKey, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource, removeFileAndTransactions, updateTransactionCategory)
import Database.TransactionSource (addTransactionSource, getAllTransactionSources, getTransactionSource, removeTransactionSource, updateTransactionSource)
import Database.UploadConfiguration (getAllUploadConfigs, getUploadConfigById)
import Sankey (generateSankeyData)
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, header, html, json, pathParam, post, redirect, text)
import Worker.ParseFileJob (asyncFileProcess, resetAllFileProcessingJobs, resetFileProcessingJob)

registerFileRoutes :: ConnectionPool -> ScottyM ()
registerFileRoutes pool = do
  post "/reprocess-file/:jobId" $ requireUser pool $ \user -> do
    jobIdText <- Web.Scotty.pathParam "jobId"

    let jobId = toSqlKey $ read jobIdText

    liftIO $ print "reprocessing"
    resetFileProcessingJob user jobId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo

  post "/delete-file/" $ requireUser pool $ \user -> do
    fileIdText <- Web.Scotty.formParam "fId"

    let fileId = toSqlKey $ read fileIdText

    liftIO $ removeFileAndTransactions user fileId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo

  post "/reprocess-all/" $ requireUser pool $ \user -> do
    liftIO $ print "reprocessing"
    resetAllFileProcessingJobs user

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo
