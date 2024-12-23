module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import Bank

main :: IO ()
main = do
  records <- parseBankFile "bank_statement.csv"
  let summary = aggregateByCategory records
      htmlOutput = generateHTML summary

  print summary
  TIO.writeFile "expense_summary.html" htmlOutput
  putStrLn "Expense summary generated: expense_summary.html"


