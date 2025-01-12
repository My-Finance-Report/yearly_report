{-# LANGUAGE NamedFieldPuns #-}

module Sankey where

import Data.List (sortBy)
import Data.Map
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import Database.Persist
import Database.Models
import Types

buildSankeyLinks ::
  (Entity TransactionSource, Entity Category, Entity TransactionSource) ->
  Map (Entity TransactionSource) AggregatedTransactions ->
  [(Text, Text, Double)]
buildSankeyLinks (sourceSource, sourceCategory, targetSource) aggregatedTransactions =
  case Map.lookup targetSource aggregatedTransactions of
    Just targetTransactions ->
      let categoryTotals = Map.map (sum . Prelude.map (sankeyAmount . transaction)) targetTransactions
          sankeyLinks =
            Prelude.map
              (\(category, total) -> (categoryName (entityVal sourceCategory), category, total))
              (Map.toList categoryTotals)
       in sankeyLinks
    Nothing -> []

generateSankeyData ::
  Map (Entity TransactionSource) [CategorizedTransaction] ->
  FullSankeyConfig ->
  [(Text, Text, Double)]
generateSankeyData transactions config =
  let aggregatedTransactions = groupByBlahForAll transactions (categoryName . category)

      FullSankeyConfig {inputs, linkages, mapKeyFunction} = config

      -- Process input flows
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
                              ( categoryName (entityVal category),
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
signedAmount txn = case transactionKind txn of
  Deposit -> transactionAmount txn
  Withdrawal -> negate (transactionAmount txn)