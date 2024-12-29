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
    conn <- open dbPath  -- this syntax is "special" for IO actions. think of it like going out innto the world and coming back with a value
    -- this makes the result "impure" since we cant be sure that the results of open are imndepotent (sp?)
    execute_ conn "CREATE TABLE IF NOT EXISTS transactions (id INTEGER PRIMARY KEY AUTOINCREMENT, description TEXT UNIQUE NOT NULL, category TEXT NOT NULL)"
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

insertTransaction :: FilePath -> Text -> Text -> IO ()
insertTransaction dbPath description category = do
    -- perform io syntax
    conn <- open dbPath
    execute conn "INSERT OR IGNORE INTO transactions (description, category) VALUES (?, ?)" (description, category)
    close conn
