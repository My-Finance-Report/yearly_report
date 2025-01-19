{-# LANGUAGE NamedFieldPuns #-}

module Sankey where

import Data.List (sortBy)
import Data.Map
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import Database.Models
import Database.Persist
import Text.Printf (printf)
import Types

truncateTo2 :: Double -> Double
truncateTo2 x = read (printf "%.2f" x) :: Double

buildSankeyLinks ::
  [(Entity TransactionSource, Entity Category, Entity TransactionSource)] ->
  Map (Entity TransactionSource) AggregatedTransactions ->
  [(Text, Text, Double)]
buildSankeyLinks linkages aggregatedTransactions =
  concatMap buildSingleLinkage linkages
  where
    buildSingleLinkage :: (Entity TransactionSource, Entity Category, Entity TransactionSource) -> [(Text, Text, Double)]
    buildSingleLinkage (sourceSource, sourceCategory, targetSource) =
      case Map.lookup targetSource aggregatedTransactions of
        Just targetTransactions ->
          let categoryTotals = Map.map (truncateTo2 . sum . Prelude.map (sankeyAmount . entityVal . transaction)) targetTransactions
           in [ (formatSankeyLabel sourceSource sourceCategory, transactionSourceName (entityVal targetSource) <> pack ":" <> category, total)
                | (category, total) <- Map.toList categoryTotals
              ]
        Nothing -> []

formatSankeyLabel :: Entity TransactionSource -> Entity Category -> Text
formatSankeyLabel source category =
  transactionSourceName (entityVal source) <> pack ":" <> categoryName (entityVal category)

generateSankeyData ::
  Map (Entity TransactionSource) [CategorizedTransaction] ->
  FullSankeyConfig ->
  [(Text, Text, Double)]
generateSankeyData transactions config =
  let aggregatedTransactions = groupByBlahForAll transactions (categoryName . entityVal . category)

      FullSankeyConfig {inputs, linkages} = config

      inputFlows = concatMap processInput inputs
        where
          processInput :: (Entity TransactionSource, Entity Category) -> [(Text, Text, Double)]
          processInput (source, category) =
            case Map.lookup source aggregatedTransactions of
              Just categorizedByCategory ->
                case Map.lookup (categoryName (entityVal category)) categorizedByCategory of
                  Just transactionsForCategory ->
                    let subCategoryFlows =
                          Map.toList $
                            Map.filterWithKey
                              (\catName _ -> catName /= categoryName (entityVal category))
                              categorizedByCategory
                     in Prelude.map
                          ( \(subCatName, txns) ->
                              ( formatSankeyLabel source category,
                                transactionSourceName (entityVal source) <> pack ":" <> subCatName,
                                truncateTo2 . sum $ Prelude.map (sankeyAmount . entityVal . transaction) txns
                              )
                          )
                          subCategoryFlows
                  Nothing -> []
              Nothing -> []

      combinedFlows = inputFlows ++ buildSankeyLinks linkages aggregatedTransactions
      sortedFlows = sortBy (comparing (Down . second)) combinedFlows
      prunedFlows = Prelude.filter (\flow -> third flow /= 0) sortedFlows
   in prunedFlows
  where
    second (_, x, _) = x
    third :: (Text, Text, Double) -> Double
    third (_, _, x) = x

sankeyAmount :: Transaction -> Double
sankeyAmount txn = abs $ signedAmount txn

signedAmount :: Transaction -> Double
signedAmount txn = case transactionKind txn of
  Deposit -> 0
  Withdrawal -> negate (transactionAmount txn)
