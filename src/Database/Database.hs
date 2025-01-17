{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Database (seedDatabase, updateUserOnboardingStep, getDemoUser) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Database.Category
import Database.ConnectionPool
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Database.TransactionSource
import Types

seedDatabase :: Entity User -> IO ()
seedDatabase user = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        bankSourceId <- ensureTransactionSourceExists user "Bank"
        ccSourceId <- ensureTransactionSourceExists user "Credit Card"

        ensureCategoriesExist
          user
          bankSourceId
          ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

        ensureCategoriesExist
          user
          ccSourceId
          ["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food"]

        liftIO $ putStrLn "Database seeded successfully!"
    )
    pool

updateUserOnboardingStep :: Entity User -> Maybe Int -> IO ()
updateUserOnboardingStep user step = do
  pool <- getConnectionPool
  runSqlPool (updateUserStep user step) pool
  where
    updateUserStep user step = update (entityKey user) [UserOnboardingStep =. step]

getDemoUser :: (MonadUnliftIO m) => m (Entity User)
getDemoUser = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool queryDemoUser pool
  liftIO $ maybe (throwIO $ userError "Demo user not found!") pure result
  where
    demoId = toSqlKey $ read "1"
    queryDemoUser = selectFirst [UserId ==. demoId] []