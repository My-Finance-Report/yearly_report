{-# LANGUAGE OverloadedStrings #-}

module Database (
    initializeDatabase
    ,getAllTransactions
    ,insertTransaction
    ,isFileProcessed
    ,markFileAsProcessed
) where

import Database.SQLite.Simple
import Data.Text (Text)
import Types (CategorizedTransaction(..), Transaction(..), TransactionKind(..))
import Data.Maybe (mapMaybe)
import Data.Time
import qualified Data.Text as T


initializeDatabase :: FilePath -> IO ()
initializeDatabase dbPath = do
    conn <- open dbPath
    execute_ conn
        "CREATE TABLE IF NOT EXISTS transactions (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ description TEXT NOT NULL, \
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
    parseResult (desc, cat, dateText, amt, src) = do
        kind <- parseTransactionKind src
        case parseTimeM True defaultTimeLocale "%m/%d/%Y" (T.unpack dateText) :: Maybe Day of
            Just parsedDate ->
                Just CategorizedTransaction
                    { transaction = Transaction 
                        { description = desc
                        , amount = amt
                        , transactionDate = parsedDate
                        }
                    , category = cat
                    , transactionKind = kind
                    }
            Nothing -> Nothing  -- Skip entries with invalid dates

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
