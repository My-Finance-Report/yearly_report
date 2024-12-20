{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (trace)

-- Define a data structure for the parsed bank record
data BankRecord = BankRecord
  { date :: Text
  , check_number :: Text
  , description :: Text
  , deposits :: Maybe Double
  , withdrawals :: Maybe Double
  , balance :: Maybe Double
  } deriving (Show, Eq)

type CategorySummary = Map.Map Text Double

parseLine :: Text -> Maybe BankRecord
parseLine line =
  case splitLine of
    (date:check_number:desc:deposits:withdrawals:balance:rest) ->
      let 
          amounts = [deposits, withdrawals, balance] 
          [deposit, withdrawal, parsed_balance] = map parseAmount amounts
      in Just $ BankRecord date check_number desc deposit withdrawal parsed_balance
    _ -> Nothing
  where
    splitLine = T.splitOn "," line
    isAmount = T.any (`elem` ("0123456789.," :: String)) 

-- Parse a single amount from text
parseAmount :: Text -> Maybe Double
parseAmount t = 
  let cleanText = T.replace "," "" t

  in trace ("Clean text: " ++ T.unpack cleanText) $ case reads (T.unpack cleanText) of
       [(x, "")] -> Just x
       _ -> Nothing


parseBankFile :: FilePath -> IO [BankRecord]
parseBankFile filePath = do
  content <- TIO.readFile filePath
  let lines = T.lines content
  return $ mapMaybe parseLine lines

categorize :: Text -> Text
categorize desc
  | "Vanguard" `T.isInfixOf` desc = "Investments"
  | "Payroll" `T.isInfixOf` desc = "Income"
  | "Transfer" `T.isInfixOf` desc = "Transfers"
  | "Capital One" `T.isInfixOf` desc = "Credit Card Payments"
  | "State Farm" `T.isInfixOf` desc = "Insurance"
  | otherwise = "Other"

aggregateByCategory :: [BankRecord] -> CategorySummary
aggregateByCategory = foldl'
    (\acc record ->
      let category = categorize (description record)
          withdrawal_amount = fromMaybe 0 (withdrawals record)
          deposit_amount = fromMaybe 0 (deposits record)
          value = case () of
                   _ | withdrawal_amount /= 0 -> withdrawal_amount
                     | deposit_amount /= 0    -> deposit_amount
                     | otherwise              -> 0
      in trace ("Aggregating: " ++ show (description record, value, category)) $
         Map.insertWith (+) category value acc)
    Map.empty

generateHTML :: CategorySummary -> Text
generateHTML summary =
  let rows = Map.foldrWithKey (\category total acc ->
                acc <> "<tr><td>" <> category <> "</td><td>" <> T.pack (show total) <> "</td></tr>\n"
              ) "" summary
  in "<!DOCTYPE html>\n<html>\n<head>\n<title>Expense Summary</title>\n</head>\n<body>\n" <>
     "<h1>Expense Summary</h1>\n<table border='1'>\n<tr><th>Category</th><th>Total</th></tr>\n" <>
     rows <>
     "</table>\n</body>\n</html>"


-- really we need to figure out a way to process the bank statements themselves so that 
-- we can get them as CSVs
-- need to watch out for commas in desc if we use llm to convert to csv

-- Example usage
main :: IO ()
main = do
  records <- parseBankFile "bank_statement.csv"
  let summary = aggregateByCategory records
      htmlOutput = generateHTML summary

  print summary
  print records
  TIO.writeFile "expense_summary.html" htmlOutput
  putStrLn "Expense summary generated: expense_summary.html"
