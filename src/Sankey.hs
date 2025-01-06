{-# LANGUAGE NamedFieldPuns #-}

module Sankey where

import Data.List (sortBy)
import Data.Map
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import Types

buildSankeyLinks ::
  (TransactionSource, Category, TransactionSource) ->
  Map TransactionSource AggregatedTransactions ->
  [(Text, Text, Double)]
buildSankeyLinks (sourceSource, sourceCategory, targetSource) aggregatedTransactions =
  case Data.Map.lookup targetSource aggregatedTransactions of
    Just targetTransactions ->
      -- Sum the amounts for each key in the map
      let categoryTotals = Data.Map.map (sum . Prelude.map (signedAmount . transaction)) targetTransactions
          -- Generate Sankey data: [sourceCategory, key, value]
          sankeyLinks =
            Prelude.map
              (\(category, total) -> (categoryName sourceCategory, category, total))
              (Data.Map.toList categoryTotals)
       in sankeyLinks
    Nothing -> []

generateSankeyData ::
  Map TransactionSource [CategorizedTransaction] ->
  SankeyConfig ->
  [(Text, Text, Double)]
generateSankeyData transactions config =
  let -- Group transactions by category name
      aggregatedTransactions = groupByBlahForAll transactions (categoryName . category)

      SankeyConfig {inputs, linkages, mapKeyFunction} = config

      -- Process input flows
      inputFlows = concatMap processInput inputs
        where
          processInput :: (TransactionSource, Category) -> [(Text, Text, Double)]
          processInput (source, category) =
            case Map.lookup source aggregatedTransactions of
              Just categorizedByCategory ->
                case Map.lookup (categoryName category) categorizedByCategory of
                  Just transactionsForCategory ->
                    let subCategoryFlows =
                          Map.toList $
                            Map.filterWithKey
                              (\catName _ -> catName /= categoryName category)
                              categorizedByCategory
                     in Prelude.map
                          ( \(subCatName, txns) ->
                              ( categoryName category,
                                subCatName,
                                sum $ Prelude.map (sankeyAmount . transaction) txns
                              )
                          )
                          subCategoryFlows
                  Nothing -> []
              Nothing -> []

      -- Build the Sankey links
      combinedFlows = inputFlows ++ buildSankeyLinks linkages aggregatedTransactions

      -- Sort the flows by the third element (weight) in descending order
      sortedFlows = sortBy (comparing (Down . third)) combinedFlows
   in sortedFlows
  where
    third (_, _, x) = x

sankeyAmount :: Transaction -> Double
sankeyAmount txn = abs $ signedAmount txn

signedAmount :: Transaction -> Double
signedAmount txn = case kind txn of
  Deposit -> amount txn
  Withdrawal -> negate (amount txn)