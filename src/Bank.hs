{-# LANGUAGE OverloadedStrings #-}

module Bank
  ( TransactionType(..)
  , BankRecord(..)
  , aggregateByCategory
  , categorize
  , generateBankHtml
  , parseLine
  , parseBankFile
  ) where

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (trace)

data TransactionType
  = Deposit Double
  | Withdrawl Double
  deriving (Show, Eq)



-- Define a data structure for the parsed bank record
data BankRecord = BankRecord
  { date :: Text
  , check_number :: Text
  , description :: Text
  , transaction :: TransactionType
  , balance :: Maybe Double
  } deriving (Show, Eq)

type CategorySummary = Map.Map Text Double


parseLine :: Text -> Maybe BankRecord  -- function  type
parseLine line =  -- actual function definition
    case broken_up_line of -- we start a case statement
      (date:check_number:desc:deposites:withdrawls:balance:the_rest) -> do -- if the split line has >= 7 elements, hit this do statement
        let deposit  = parseAmount deposites -- define 3 variables using a helper, these are either Double or Nothing
            withdrawl = parseAmount withdrawls --aka Maybe Double
            the_balance = parseAmount balance 
        transaction <- case (withdrawl, deposit) of --start another case statement passing withdrawl and deposite
            (Just w , Nothing) -> Just (Withdrawl w) -- match a withdrawl and no deposite, set w's "type" to be Withdrawl, why Just?
            (Nothing, Just d) -> Just (Deposit d) -- same as above but with deposite
            _ -> Nothing -- basically a catch all incase there is (Just w, Just d) or (Nothing, Nothing), which is invalid state
        Just ( BankRecord date check_number desc transaction the_balance ) -- now that we have types "determined", make the object. <$> is the infix for fmap 
      _ -> Nothing -- handle a split line that is less than 7 elements
    where broken_up_line = T.splitOn "," line -- not define the split line as splitting on ","




-- Helper to parse an amount from text
parseAmount :: Text -> Maybe Double
parseAmount t =
    if T.null t || not (T.any (`elem` ("0123456789." :: String)) t)
        then Nothing
        else case reads (T.unpack t) of
        [(n, "")] -> Just n
        _         -> Nothing

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
          value = case transaction record of
                    Deposit amount   -> amount
                    Withdrawl amount -> amount
      in trace ("Aggregating: " ++ show (description record, value, category)) $
         Map.insertWith (+) category value acc)
    Map.empty


generateBankHtml :: CategorySummary -> Text
generateBankHtml summary =
    let tableRows = Map.foldrWithKey (\category total acc ->
            acc <> "<tr><td>" <> category <> "</td><td>" <> T.pack (show total) <> "</td></tr>\n"
            ) "" summary
    in "<table>\n" <> tableRows <> "</table>\n"

