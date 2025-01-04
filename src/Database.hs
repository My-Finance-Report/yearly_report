{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initializeDatabase,
    getAllTransactions,
    updateTransactionCategory,
    insertTransaction,
    isFileProcessed,
    fetchPdfRecord,
    markFileAsProcessed,
    getAllFilenames,
    insertPdfRecord,
    getCategoriesBySource,
    getTransactionSource,
    getAllTransactionSources,
    getTransactionsByFilename,
    insertTransactionSource,
    insertCategory,
  )
where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Types

-- TODO this needs to be indepodent
initializeSourcesAndCategories :: FilePath -> IO ()
initializeSourcesAndCategories dbPath = do
  bankSource <- insertTransactionSource dbPath "Bank"
  ccSource <- insertTransactionSource dbPath "CreditCard"

  -- Insert categories for Bank
  mapM_
    (insertCategory dbPath `flip` sourceId bankSource)
    ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

  -- Insert categories for CreditCard
  mapM_
    (insertCategory dbPath `flip` sourceId ccSource)
    ["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food"]

initializeDatabase :: FilePath -> IO ()
initializeDatabase dbPath = do
  conn <- open dbPath

  -- Create transaction_sources table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS transaction_sources (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ name TEXT NOT NULL UNIQUE)"

  -- Create categories table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS categories (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ name TEXT NOT NULL, \
    \ source_id INTEGER NOT NULL, \
    \ FOREIGN KEY (source_id) REFERENCES transaction_sources (id), \
    \ UNIQUE (name, source_id))"

  -- Create transactions table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS transactions (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ description TEXT NOT NULL, \
    \ category_id INTEGER NOT NULL, \
    \ date_of_transaction TEXT NOT NULL, \
    \ amount REAL NOT NULL, \
    \ transaction_source_id INTEGER NOT NULL, \
    \ kind TEXT NOT NULL, \
    \ filename TEXT, \
    \ FOREIGN KEY (category_id) REFERENCES categories (id), \
    \ FOREIGN KEY (transaction_source_id) REFERENCES transaction_sources (id))"

  -- Create processed_files table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS processed_files (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ filename TEXT UNIQUE NOT NULL)"

  -- Create uploaded_pdfs table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS uploaded_pdfs (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ filename TEXT NOT NULL, \
    \ raw_content TEXT NOT NULL, \
    \ upload_time TEXT NOT NULL)"

  close conn
  initializeSourcesAndCategories dbPath

-- Fetch all filenames from transactions
getAllFilenames :: FilePath -> IO [Text]
getAllFilenames dbPath = do
  conn <- open dbPath
  results <- query_ conn "SELECT DISTINCT filename FROM transactions" :: IO [Only Text]
  close conn
  return $ map fromOnly results

getTransactionSource :: FilePath -> Int -> IO TransactionSource
getTransactionSource dbPath txnId = do
  conn <- open dbPath
  rows <- query conn "SELECT id, name FROM transaction_sources WHERE id = ?" (Only txnId) :: IO [(Int, Text)]
  close conn
  case rows of
    [(sourceId, sourceName)] -> return $ TransactionSource sourceId sourceName
    [] -> error $ "No transaction source found with ID: " ++ show txnId
    _ -> error $ "Multiple transaction sources found with ID: " ++ show txnId

-- Fetch all transaction sources
getAllTransactionSources :: FilePath -> IO [TransactionSource]
getAllTransactionSources dbPath = do
  conn <- open dbPath
  rows <- query_ conn "SELECT id, name FROM transaction_sources" :: IO [(Int, Text)]
  close conn
  return $ map (uncurry TransactionSource) rows

-- Insert a new transaction source
insertTransactionSource :: FilePath -> Text -> IO TransactionSource
insertTransactionSource dbPath sourceName = do
  conn <- open dbPath
  execute conn "INSERT OR IGNORE INTO transaction_sources (name) VALUES (?)" (Only sourceName)
  rowId <- lastInsertRowId conn
  close conn
  return $ TransactionSource {sourceId = fromIntegral rowId, sourceName = sourceName}

getAllTransactions :: FilePath -> IO [CategorizedTransaction]
getAllTransactions dbPath = do
  conn <- open dbPath
  results <-
    query
      conn
      "SELECT t.id, t.description, c.id, t.date_of_transaction, \
      \ t.amount, ts.id, ts.name, t.kind \
      \FROM transactions t \
      \INNER JOIN transaction_sources ts ON t.transaction_source_id = ts.id \
      \INNER JOIN categories c ON t.category_id = c.id"
      () ::
      IO [(Maybe Int, Text, Int, Text, Double, Int, Text, Text)]
  close conn
  mapM parseResult results
  where
    parseResult (id, desc, catId, dateText, amt, sourceId, sourceName, kind) = do
      let transactionSource = TransactionSource sourceId sourceName
          parsedKind = parseTransactionKind kind
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText) of
        Just parsedDate ->
          return
            CategorizedTransaction
              { transaction =
                  Transaction
                    { description = desc,
                      amount = amt,
                      transactionDate = parsedDate,
                      kind = parsedKind
                    },
                category = T.pack (show catId), -- Convert category_id to Text
                transactionId = id,
                transactionSource = transactionSource
              }
        Nothing -> fail $ "Error parsing date: " <> T.unpack dateText

getTransactionsByFilename :: FilePath -> FilePath -> IO [CategorizedTransaction]
getTransactionsByFilename dbPath filename = do
  conn <- open dbPath
  results <-
    query
      conn
      "SELECT t.id, t.description, c.id, t.date_of_transaction, \
      \ t.amount, ts.id, ts.name, t.kind \
      \FROM transactions t \
      \INNER JOIN transaction_sources ts ON t.transaction_source_id = ts.id \
      \INNER JOIN categories c ON t.category_id = c.id \
      \WHERE lower(t.filename) = lower(?)"
      (Only filename) ::
      IO [(Maybe Int, Text, Int, Text, Double, Int, Text, Text)]
  close conn
  mapM parseResult results
  where
    parseResult (id, desc, catId, dateText, amt, sourceId, sourceName, kind) = do
      let transactionSource = TransactionSource sourceId sourceName
          parsedKind = parseTransactionKind kind
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText) of
        Just parsedDate ->
          return
            CategorizedTransaction
              { transaction =
                  Transaction
                    { description = desc,
                      amount = amt,
                      transactionDate = parsedDate,
                      kind = parsedKind
                    },
                category = T.pack (show catId), -- Convert category_id to Text
                transactionId = id,
                transactionSource = transactionSource
              }
        Nothing -> fail $ "Error parsing date: " <> T.unpack dateText

parseTransactionKind :: Text -> TransactionKind
parseTransactionKind "Withdrawal" = Withdrawal
parseTransactionKind "Deposit" = Deposit
parseTransactionKind _ = error "Invalid transaction kind"

insertTransaction :: FilePath -> CategorizedTransaction -> FilePath -> IO Int
insertTransaction dbPath categorizedTransaction sourceFile = do
  conn <- open dbPath
  let tx = transaction categorizedTransaction
      src = transactionSource categorizedTransaction
      categoryId = category categorizedTransaction -- Assume category is now the ID
  execute
    conn
    "INSERT INTO transactions \
    \ (description, category_id, date_of_transaction, amount, transaction_source_id, filename, kind) \
    \ VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( description tx,
      categoryId,
      transactionDate tx,
      amount tx,
      sourceId src,
      sourceFile,
      show $ kind tx
    )
  rowId <- lastInsertRowId conn
  close conn
  return $ fromIntegral rowId

-- Fetch a PDF record by its ID
fetchPdfRecord :: FilePath -> Int -> IO (Text, Text)
fetchPdfRecord dbPath pdfId = do
  conn <- open dbPath
  rows <- query conn "SELECT filename, raw_content FROM uploaded_pdfs WHERE id = ?" (Only pdfId) :: IO [(Text, Text)]
  close conn
  case rows of
    [] -> fail $ "No PDF found with id=" ++ show pdfId
    (x : _) -> return x

-- Insert a new PDF record
insertPdfRecord :: FilePath -> Text -> Text -> IO Int
insertPdfRecord dbPath filename rawContent = do
  now <- getCurrentTime
  let timeString = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  conn <- open dbPath
  execute conn "INSERT INTO uploaded_pdfs (filename, raw_content, upload_time) VALUES (?, ?, ?)" (filename, rawContent, timeString)
  rowId <- lastInsertRowId conn
  close conn
  return $ fromIntegral rowId

-- Check if a file has already been processed
isFileProcessed :: FilePath -> Int -> IO Bool
isFileProcessed dbPath id = do
  conn <- open dbPath
  results <- query conn "SELECT 1 FROM uploaded_pdfs WHERE id = ?" (Only id) :: IO [Only Int]
  close conn
  return $ not (null results)

-- Mark a file as processed
markFileAsProcessed :: FilePath -> Text -> IO ()
markFileAsProcessed dbPath filename = do
  conn <- open dbPath
  execute conn "INSERT OR IGNORE INTO processed_files (filename) VALUES (?)" (Only filename)
  close conn

-- Update the category of a transaction
updateTransactionCategory :: FilePath -> Int -> Text -> IO ()
updateTransactionCategory dbPath tId newCat = do
  conn <- open dbPath
  execute conn "UPDATE transactions SET category = ? WHERE id = ?" (newCat, tId)
  close conn

-- Insert a category
insertCategory :: FilePath -> Text -> Int -> IO Int
insertCategory dbPath categoryName sourceId = do
  conn <- open dbPath
  execute
    conn
    "INSERT OR IGNORE INTO categories (name, source_id) VALUES (?, ?)"
    (categoryName, sourceId)
  rowId <- lastInsertRowId conn
  close conn
  return $ fromIntegral rowId

getCategoriesBySource :: FilePath -> Int -> IO [Category]
getCategoriesBySource dbPath sourceId = do
  conn <- open dbPath
  rows <-
    query
      conn
      "SELECT id, name, source_id FROM categories WHERE source_id = ?"
      (Only sourceId) ::
      IO [(Int, Text, Int)]
  close conn
  return $ map (\(catId, name, srcId) -> Category catId name srcId) rows
