{-# LANGUAGE OverloadedStrings #-}

module Database (
    initializeDatabase
    , getCategory
    , insertTransaction
) where

import Database.SQLite.Simple
import Data.Text (Text)

initializeDatabase :: FilePath -> IO ()
initializeDatabase dbPath = do
    conn <- open dbPath
    execute_ conn
        "CREATE TABLE IF NOT EXISTS transactions (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ description TEXT UNIQUE NOT NULL, \
        \ category TEXT, \
        \ date_of_transaction TEXT NOT NULL, \
        \ source TEXT NOT NULL)"
    close conn

getCategory :: FilePath -> Text -> IO (Maybe Text)
getCategory dbPath description = do
    conn <- open dbPath -- do io syntax
    -- same here
    results <- query conn "SELECT category FROM transactions WHERE description = ?" (Only description) :: IO [Only Text]
    close conn

    return $ case results of
        [Only category] -> Just category
        _ -> Nothing

insertTransaction :: FilePath -> Text -> Text -> Text -> Text -> IO ()
insertTransaction dbPath description category dateOfTransaction source = do
    conn <- open dbPath
    execute conn
        "INSERT OR IGNORE INTO transactions (description, category, date_of_transaction, source) VALUES (?, ?, ?, ?)"
        (description, category, dateOfTransaction, source)
    close conn

