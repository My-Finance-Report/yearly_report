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
    getTransactionSourceText,
    getAllTransactionSources,
    getTransactionsByFilename,
    insertTransactionSource,
    insertCategory,
    persistUploadConfiguration,
    getUploadConfiguration,
    getTransactionSourceFiles,
    loadSankeyConfig,
    saveSankeyConfig,
    getCategory,
  )
where

import Control.Exception (SomeException (SomeException))
import Control.Exception.Base (try)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Types

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

  -- Create upload_configuration table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS upload_configuration (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ filename_regex TEXT, \
    \ start_keyword TEXT, \
    \ end_keyword TEXT, \
    \ transaction_source_id INTEGER NOT NULL, \
    \ FOREIGN KEY (transaction_source_id) REFERENCES transaction_sources (id), \
    \ UNIQUE (transaction_source_id))"

  -- Create sankey_config table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS sankey_config (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ name TEXT NOT NULL UNIQUE)"

  -- Create sankey_inputs table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS sankey_inputs (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ config_id INTEGER NOT NULL, \
    \ source_id INTEGER NOT NULL, \
    \ category_id INTEGER NOT NULL, \
    \ FOREIGN KEY (config_id) REFERENCES sankey_config (id), \
    \ FOREIGN KEY (category_id) REFERENCES categories (id), \
    \ FOREIGN KEY (source_id) REFERENCES transaction_sources (id))"

  -- Create sankey_linkages table
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS sankey_linkages (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ config_id INTEGER NOT NULL, \
    \ source_id INTEGER NOT NULL, \
    \ category_id INTEGER NOT NULL, \
    \ target_source_id INTEGER NOT NULL, \
    \ FOREIGN KEY (config_id) REFERENCES sankey_config (id), \
    \ FOREIGN KEY (source_id) REFERENCES transaction_sources (id), \
    \ FOREIGN KEY (category_id) REFERENCES categories (id), \
    \ FOREIGN KEY (target_source_id) REFERENCES transaction_sources (id))"

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

getTransactionSourceText :: FilePath -> Text -> IO TransactionSource
getTransactionSourceText dbPath txnName = do
  conn <- open dbPath
  rows <- query conn "SELECT id, name FROM transaction_sources WHERE name = ?" (Only txnName) :: IO [(Int, Text)]
  close conn
  case rows of
    [(sourceId, sourceName)] -> return $ TransactionSource sourceId sourceName
    [] -> error $ "No transaction source found with ID: " ++ show txnName
    _ -> error $ "Multiple transaction sources found with ID: " ++ show txnName

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
      "SELECT t.id, t.description, c.id, c.name, t.date_of_transaction, \
      \ t.amount, ts.id, ts.name, t.kind \
      \FROM transactions t \
      \INNER JOIN transaction_sources ts ON t.transaction_source_id = ts.id \
      \INNER JOIN categories c ON t.category_id = c.id"
      () ::
      IO [(Maybe Int, Text, Int, Text, Text, Double, Int, Text, Text)]
  close conn
  mapM parseResult results
  where
    parseResult (id, desc, catId, catName, dateText, amt, sourceId, sourceName, kind) = do
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
                category = Category {categoryId = catId, categoryName = catName, transactionSource = transactionSource},
                transactionId = id
              }
        Nothing -> fail $ "Error parsing date: " <> T.unpack dateText

getTransactionsByFilename :: FilePath -> FilePath -> IO [CategorizedTransaction]
getTransactionsByFilename dbPath filename = do
  conn <- open dbPath
  results <-
    query
      conn
      "SELECT t.id, t.description, c.id, c.name, t.date_of_transaction, \
      \ t.amount, ts.id, ts.name, t.kind \
      \FROM transactions t \
      \INNER JOIN transaction_sources ts ON t.transaction_source_id = ts.id \
      \INNER JOIN categories c ON t.category_id = c.id \
      \WHERE lower(t.filename) = lower(?)"
      (Only filename) ::
      IO [(Maybe Int, Text, Int, Text, Text, Double, Int, Text, Text)]
  close conn
  mapM parseResult results
  where
    parseResult (id, desc, catId, catName, dateText, amt, sourceId, sourceName, kind) = do
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
                category = Category {categoryId = catId, categoryName = catName, transactionSource = transactionSource},
                transactionId = id
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
      src = transactionSource (category categorizedTransaction)
      catId = categoryId (category categorizedTransaction)
  execute
    conn
    "INSERT INTO transactions \
    \ (description, category_id, date_of_transaction, amount, transaction_source_id, filename, kind) \
    \ VALUES (?, ?, ?, ?, ?, ?, ?)"
    ( description tx,
      catId,
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
  results <- query conn "SELECT 1 FROM processed_files WHERE id = ?" (Only id) :: IO [Only Int]
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
  let sql =
        "SELECT c.id, c.name, s.id, s.name \
        \FROM categories c \
        \JOIN transaction_sources s ON c.source_id = s.id \
        \WHERE c.source_id = ?"
  categories <- query conn sql (Only sourceId) :: IO [Category]
  close conn
  return categories

getCategory :: FilePath -> Int -> IO Category
getCategory dbPath catId = do
  conn <- open dbPath
  let sql =
        "SELECT c.id, c.name, s.id, s.name \
        \FROM categories c \
        \JOIN transaction_sources s ON c.source_id = s.id \
        \WHERE c.id = ?"
  rows <- query conn sql (Only catId) :: IO [Category]
  close conn
  case rows of
    [] -> fail $ "No category found with id=" ++ show catId
    (x : _) -> return x

persistUploadConfiguration :: FilePath -> T.Text -> T.Text -> Int -> T.Text -> IO ()
persistUploadConfiguration dbPath startKeyword endKeyword txnSourceId filenameRegex = do
  conn <- open dbPath

  execute
    conn
    "INSERT OR IGNORE INTO upload_configuration (start_keyword, end_keyword, transaction_source_id, filename_regex) \
    \VALUES (?, ?, ?, ?);"
    (startKeyword, endKeyword, txnSourceId, filenameRegex)

  close conn
  putStrLn "Connection closed."

getUploadConfiguration :: FilePath -> FilePath -> IO (Maybe UploadConfiguration)
getUploadConfiguration dbPath filename = do
  conn <- open dbPath
  rows <-
    query
      conn
      "SELECT start_keyword, end_keyword, transaction_source_id, filename_regex \
      \FROM upload_configuration \
      \WHERE LOWER(?) LIKE '%' || LOWER(filename_regex) || '%'"
      (Only filename) ::
      IO [UploadConfiguration]
  close conn
  return $ listToMaybe rows

getTransactionSourceFiles ::
  FilePath ->
  IO (Map.Map TransactionSource [Text])
getTransactionSourceFiles dbPath = do
  conn <- open dbPath
  rows <-
    query_
      conn
      "SELECT DISTINCT ts.id, ts.name, t.filename \
      \FROM transaction_sources ts \
      \JOIN transactions t ON ts.id = t.transaction_source_id \
      \WHERE t.filename IS NOT NULL" ::
      IO [(Int, Text, Text)]
  close conn
  let grouped =
        Map.fromListWith (++) $
          [ (TransactionSource sourceId sourceName, [filename])
            | (sourceId, sourceName, filename) <- rows
          ]
  return grouped

saveSankeyConfig :: FilePath -> SankeyConfig -> IO Int
saveSankeyConfig dbPath config = do
  conn <- open dbPath

  -- Upsert into sankey_config
  execute
    conn
    "INSERT INTO sankey_config (name) VALUES (?) \
    \ON CONFLICT(name) DO UPDATE SET name = excluded.name"
    (Only $ configName config)

  [Only configId] <-
    query
      conn
      "SELECT id FROM sankey_config WHERE name = ?"
      (Only $ configName config) ::
      IO [Only Int]

  execute conn "DELETE FROM sankey_inputs WHERE config_id = ?" (Only configId)
  execute conn "DELETE FROM sankey_linkages WHERE config_id = ?" (Only configId)

  forM_ (inputs config) $ \(TransactionSource sourceId _, Category categoryId _ _) ->
    execute
      conn
      "INSERT INTO sankey_inputs (config_id, source_id, category_id) VALUES (?, ?, ?)"
      (configId, sourceId, categoryId)

  let (TransactionSource sourceId _, Category categoryId _ _, TransactionSource targetSourceId _) = linkages config
  execute
    conn
    "INSERT INTO sankey_linkages (config_id, source_id, category_id, target_source_id) VALUES (?, ?, ?, ?)"
    (configId, sourceId, categoryId, targetSourceId)

  close conn
  return configId

loadSankeyConfig :: FilePath -> Int -> IO (Maybe SankeyConfig)
loadSankeyConfig dbPath configId = do
  conn <- open dbPath

  -- Load the configuration name
  configNameRows <-
    query
      conn
      "SELECT name FROM sankey_config WHERE id = ?"
      (Only configId) ::
      IO [Only Text]

  case configNameRows of
    [] -> do
      close conn
      return Nothing -- No configuration found
    [Only configName] -> do
      -- Load inputs
      inputs <-
        query
          conn
          "SELECT ts.id, ts.name, c.id, c.name FROM sankey_inputs si \
          \JOIN transaction_sources ts ON si.source_id = ts.id \
          \JOIN categories c ON si.category_id = c.id \
          \WHERE si.config_id = ?"
          (Only configId) ::
          IO [(Int, Text, Int, Text)]

      -- Load linkages
      linkagesRows <-
        query
          conn
          "SELECT ts.id, ts.name, c.id, c.name, tts.id, tts.name \
          \FROM sankey_linkages sl \
          \JOIN transaction_sources ts ON sl.source_id = ts.id \
          \JOIN categories c ON sl.category_id = c.id \
          \JOIN transaction_sources tts ON sl.target_source_id = tts.id \
          \WHERE sl.config_id = ?"
          (Only configId) ::
          IO [(Int, Text, Int, Text, Int, Text)]

      close conn

      case linkagesRows of
        [] -> return Nothing -- No linkages found
        [(sourceId, sourceName, categoryId, categoryName, targetSourceId, targetSourceName)] -> do
          let inputsSources =
                [ ( TransactionSource sourceId sourceName,
                    Category categoryId categoryName (TransactionSource sourceId sourceName)
                  )
                  | (sourceId, sourceName, categoryId, categoryName) <- inputs
                ]
          let linkageSources =
                ( TransactionSource sourceId sourceName,
                  Category categoryId categoryName (TransactionSource sourceId sourceName),
                  TransactionSource targetSourceId targetSourceName
                )

          return $ Just SankeyConfig {inputs = inputsSources, linkages = linkageSources, mapKeyFunction = Types.sourceName, configName = configName}
