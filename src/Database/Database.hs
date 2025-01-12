{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Database (seedDatabase) where

import Database.ConnectionPool
import Database.Models
import Control.Monad.IO.Class (liftIO)
import Types
import Database.TransactionSource
import Database.Category
import Database.Persist (Entity (..))
import Database.Persist.Postgresql

seedDatabase :: Entity User -> IO ()
seedDatabase user = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        bankSourceId <- ensureTransactionSourceExists user "Bank"
        ccSourceId <- ensureTransactionSourceExists user "CreditCard"

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

