{-# LANGUAGE OverloadedStrings #-}

module Database (
    initializeDatabase
    ,getCategory
    ,getAllTransactions
    ,insertTransaction
    ,isFileProcessed
    ,markFileAsProcessed
) where

import Database.SQLite.Simple
import Data.Text (Text)
import Types (CategorizedTransaction(..), Transaction(..), TransactionKind(..))
import Data.Maybe (mapMaybe)


initializeDatabase :: FilePath -> IO ()
initializeDatabase dbPath = do
    conn <- open dbPath
    execute_ conn
        "CREATE TABLE IF NOT EXISTS transactions (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ description TEXT UNIQUE NOT NULL, \
        \ category TEXT, \
        \ date_of_transaction TEXT NOT NULL, \
        \ amount REAL NOT NULL, \
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


getAllTransactions :: FilePath -> FilePath -> IO [CategorizedTransaction]
getAllTransactions dbPath filename = do
    conn <- open dbPath
    results <- query conn 
        "SELECT description, category, date_of_transaction, amount, source \
        \FROM transactions WHERE filename = ?" 
        (Only filename) :: IO [(Text, Text, Text, Double, Text)]
    close conn
    return $ mapMaybe parseResult results
  where
    parseResult :: (Text, Text, Text, Double, Text) -> Maybe CategorizedTransaction
    parseResult (desc, cat, date, amt, src) = do
        kind <- parseTransactionKind src
        Just CategorizedTransaction
            { transaction = Transaction 
                { description = desc
                , amount = amt
                , transactionDate = date
                }
            , category = cat
            , transactionKind = kind
            }

    parseTransactionKind :: Text -> Maybe TransactionKind
    parseTransactionKind "BankKind" = Just BankKind
    parseTransactionKind "CreditCardKind" = Just CreditCardKind
    parseTransactionKind _ = Nothing



insertTransaction :: FilePath -> CategorizedTransaction -> FilePath -> TransactionKind-> IO ()
insertTransaction dbPath categorizedTransaction sourceFile transactionKind = do
    conn <- open dbPath
    execute conn
        "INSERT OR IGNORE INTO transactions (description, category, date_of_transaction, amount, source, filename) VALUES (?, ?, ?, ?, ?, ?)"
        ( description innerTransaction
        , category categorizedTransaction
        , transactionDate innerTransaction
        , amount innerTransaction
        , show transactionKind
        , sourceFile )
    close conn
  where
    innerTransaction = transaction categorizedTransaction


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
