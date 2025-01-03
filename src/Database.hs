{-# LANGUAGE OverloadedStrings #-}

module Database (
    initializeDatabase
    ,getAllTransactions
    ,updateTransactionCategory
    ,insertTransaction
    ,isFileProcessed
    ,markFileAsProcessed
    ,getAllFilenames
) where

import Database.SQLite.Simple
import Data.Text (Text)
import Types (CategorizedTransaction(..), Transaction(..), TransactionKind(..), TransactionSource(..), PdfParseException (PdfParseException), CategorizationResponse (CategorizationResponse))
import Data.Maybe (mapMaybe)
import Data.Time
import qualified Data.Text as T
import Control.Monad (forM_)


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
        \ kind TEXT NOT NULL, \
        \ filename TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS processed_files (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ filename TEXT UNIQUE NOT NULL)"
    close conn

updateTransactionCategory :: FilePath -> Int -> T.Text -> IO ()
updateTransactionCategory dbPath tId newCat = do
  conn <- open dbPath
  execute conn
    "UPDATE transactions \
    \SET category = ? \
    \WHERE id = ?"
    (newCat, tId)
  close conn

getAllFilenames :: FilePath -> IO [T.Text]
getAllFilenames dbPath = do
    conn <- open dbPath
    results <- query_ conn "SELECT DISTINCT filename FROM transactions" 
                  :: IO [Only T.Text]
    close conn

    forM_ results print

    return (map fromOnly results)


getAllTransactions :: FilePath -> FilePath -> IO [CategorizedTransaction]
getAllTransactions dbPath filename = do
    conn <- open dbPath
    results <- query conn 
        "SELECT  id, description, category, date_of_transaction, amount, source, kind \
        \FROM transactions WHERE lower(filename) = lower(?)" 
        (Only filename) :: IO [(Int,Text, Text, Text, Double, Text, Text)]
    close conn
    print results
    print "results"
    mapM parseResult results
  where
    parseResult :: (Int,Text, Text, Text, Double, Text, Text) -> IO CategorizedTransaction
    parseResult (id, desc, cat, dateText, amt, src, kind) = do
        let source = parseTransactionSource src
        let parsedKind = parseTransactionKind kind
        print desc
        case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText) :: Maybe Day of
            Just parsedDate ->
                return CategorizedTransaction
                    { transaction = Transaction 
                        { description = desc
                        , amount = amt
                        , transactionDate = parsedDate
                        , kind = parsedKind
                        }
                    , category = cat
                    , transactionId = id
                    , transactionSource = source
                    }
            Nothing -> do
                putStrLn $ "Failed to parse date: " <> T.unpack dateText
                fail $ "Error parsing date: " <> T.unpack dateText

    parseTransactionSource :: Text -> TransactionSource
    parseTransactionSource "BankSource" = BankSource
    parseTransactionSource "CreditCardSource" = CreditCardSource
    parseTransactionSource src = error $ "Invalid transaction source: " <> T.unpack src

    parseTransactionKind :: Text -> TransactionKind
    parseTransactionKind "Withdrawal" = Withdrawal
    parseTransactionKind "Deposit" = Deposit
    parseTransactionKind kind = error $ "Invalid transaction kind: " <> T.unpack kind
 


insertTransaction :: FilePath -> CategorizedTransaction -> FilePath -> TransactionSource -> IO ()
insertTransaction dbPath categorizedTransaction sourceFile transactionSource = do
    conn <- open dbPath
    execute conn
        "INSERT INTO transactions (description, category, date_of_transaction, amount, source, filename, kind) VALUES (?, ?, ?, ?, ?, ?, ?)"
        ( description innerTransaction
        , category categorizedTransaction
        , transactionDate innerTransaction
        , amount innerTransaction
        , show transactionSource
        , sourceFile 
        , show $ kind innerTransaction)
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
