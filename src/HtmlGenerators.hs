{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators (
    generateCreditCardHtml
    , generateBankHtml)
    where



import qualified Data.Map as Map
import Data.Text 
import Data.List (sortBy)
import Data.Ord (comparing)
import Categorizer (CategorizedCreditCardTransaction(..))
import Bank (CategorySummary)
import CreditCard

generateCreditCardRow :: CategorizedCreditCardTransaction -> Text
generateCreditCardRow categorizedTransaction =
    let category = Categorizer.category categorizedTransaction
        transaction = Categorizer.transaction categorizedTransaction
        merchant = CreditCard.merchantName transaction
        amount = CreditCard.amount transaction
    in "<tr><td>" <> category <> "</td><td>" <> merchant <> "</td><td>" <> pack (show amount) <> "</td></tr>\n"


generateCreditCardHtml :: [CategorizedCreditCardTransaction] -> Text
generateCreditCardHtml categorizedTransactions =
    --     let tableRows = foldr () "" iterable ->>>> callable, starting value, iterable 

    let 
        sortedTransactions = sortBy (comparing Categorizer.category) categorizedTransactions
        tableRows = Prelude.foldr (\txn acc-> generateCreditCardRow txn <> acc) "" sortedTransactions
    in "<table>\n<tr><th>Category</th><th>Transactions</th></tr>\n" <> tableRows <> "</table>\n"



generateBankHtml :: CategorySummary  -> Text
generateBankHtml summary =
    let tableRows = Map.foldrWithKey (\category total acc ->
            acc <> "<tr><td>" <> category <> "</td><td>" <> pack (show total) <> "</td></tr>\n"
            ) "" summary
    in "<table>\n" <> tableRows <> "</table>\n"

