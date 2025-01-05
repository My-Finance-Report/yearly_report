{-# LANGUAGE NamedFieldPuns #-}

module Sankey where

import Data.List (sortBy)
import Data.Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import Types

data SankeyConfig = SankeyConfig
  { inputs :: [(TransactionSource, Text)], -- [(Source, Category)]
    linkages :: (TransactionSource, Text, TransactionSource), -- (Source, Category) -> Target Source
    mapKeyFunction :: TransactionSource -> Text -- Mapping for display names
  }

buildSankeyLinks ::
  (TransactionSource, Text, TransactionSource) ->
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
              (\(category, total) -> (sourceCategory, category, total))
              (Data.Map.toList categoryTotals)
       in sankeyLinks
    Nothing -> []

generateSankeyData ::
  Map TransactionSource [CategorizedTransaction] ->
  SankeyConfig ->
  [(Text, Text, Double)]
generateSankeyData transactions config =
  let aggregatedTransactions = groupByBlahForAll transactions (categoryName . category)
      SankeyConfig {inputs, linkages, mapKeyFunction} = config

      inputFlows = concatMap processInput inputs
        where
          processInput (source, category) =
            case Data.Map.lookup source aggregatedTransactions >>= Data.Map.lookup category of
              Just transactions ->
                let subCategoryFlows =
                      Data.Map.toList (Data.Map.filterWithKey (\k _ -> k /= category) (fromMaybe Data.Map.empty (Data.Map.lookup source aggregatedTransactions)))
                 in Prelude.map (\(cat, txns) -> (category, cat, sum $ Prelude.map (sankeyAmount . transaction) txns)) subCategoryFlows
              Nothing -> []

      combinedFlows = inputFlows ++ buildSankeyLinks linkages aggregatedTransactions

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