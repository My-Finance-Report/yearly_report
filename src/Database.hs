{-# LANGUAGE OverloadedStrings #-}

module Database (
    initializeDatabase
    ,getCategory
    ,insertTransaction
    ,isFileProcessed
    ,markFileAsProcessed
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
        \ source TEXT NOT NULL, \
        \ filename TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS processed_files (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ filename TEXT UNIQUE NOT NULL)"
    close conn

getCategory :: FilePath -> Text -> IO (Maybe Text)
getCategory dbPath description = do
    conn <- open dbPath
    results <- query conn "SELECT category FROM transactions WHERE description = ?" (Only description) :: IO [Only Text]
    close conn
    return $ case results of
        [Only category] -> Just category
        _ -> Nothing

insertTransaction :: FilePath -> Text -> Text -> Text -> Text -> Text -> IO ()
insertTransaction dbPath description category dateOfTransaction source filename = do
    conn <- open dbPath
    execute conn
        "INSERT OR IGNORE INTO transactions (description, category, date_of_transaction, source, filename) VALUES (?, ?, ?, ?, ?)"
        (description, category, dateOfTransaction, source, filename)
    close conn

isFileProcessed :: FilePath -> Text -> IO Bool
isFileProcessed dbPath filename = do
    conn <- open dbPath
    results <- query conn "SELECT 1 FROM processed_files WHERE filename = ?" (Only filename) :: IO [Only Int]
    close conn
    return $ not (null results)

markFileAsProcessed :: FilePath -> Text -> IO ()
markFileAsProcessed dbPath filename = do
    conn <- open dbPath
    execute conn "INSERT OR IGNORE INTO processed_files (filename) VALUES (?)" (Only filename)
    close conn
