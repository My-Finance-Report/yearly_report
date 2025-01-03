{-# LANGUAGE OverloadedStrings #-}

import Bank (BankRecord (..), TransactionType (..), aggregateByCategory, categorize, generateHTML, parseLine)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

instance Arbitrary TransactionType where
  arbitrary = oneof [Deposit <$> arbitrary, Withdrawl <$> arbitrary]

instance Arbitrary BankRecord where
  arbitrary =
    BankRecord
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

prop_parseAmount_nonNumeric :: Text -> Bool
prop_parseAmount_nonNumeric t =
  not (T.any (`elem` ("0123456789." :: String)) t) ==> isNothing (parseAmount t)

prop_parseAmount_validNumeric :: Double -> Bool
prop_parseAmount_validNumeric x =
  let t = T.pack (show x)
   in parseAmount t == Just x

prop_parseAmount_emptyInput :: Bool
prop_parseAmount_emptyInput = isNothing (parseAmount "")

prop_parseLine_validLine :: Text -> Text -> Text -> Double -> Maybe Double -> Bool
prop_parseLine_validLine date check_number desc amount mBalance =
  let depositLine = T.intercalate "," [date, check_number, desc, T.pack (show amount), "", maybe "" (T.pack . show) mBalance]
      withdrawLine = T.intercalate "," [date, check_number, desc, "", T.pack (show amount), maybe "" (T.pack . show) mBalance]
   in isJust (parseLine depositLine)
        && isJust (parseLine withdrawLine)

prop_parseLine_invalidLine :: Text -> Bool
prop_parseLine_invalidLine line =
  length (T.splitOn "," line) < 6 ==> isNothing (parseLine line)

prop_aggregateByCategory_correctTotals :: [BankRecord] -> Bool
prop_aggregateByCategory_correctTotals records =
  let summary = aggregateByCategory records
      recalculatedTotals =
        Map.mapWithKey
          ( \cat _ ->
              sum
                [ amount
                  | r <- records,
                    categorize (description r) == cat,
                    let amount = case transaction r of
                          Deposit a -> a
                          Withdrawl a -> a
                ]
          )
          summary
   in summary == recalculatedTotals

prop_generateHTML_matchesSummary :: CategorySummary -> Bool
prop_generateHTML_matchesSummary summary =
  let html = generateHTML summary
   in all (\(cat, total) -> T.isInfixOf cat html && T.isInfixOf (T.pack (show total)) html) (Map.toList summary)

main :: IO ()
main = do
  quickCheck prop_parseAmount_nonNumeric
  quickCheck prop_parseAmount_validNumeric
  quickCheck prop_parseAmount_emptyInput
  quickCheck prop_parseLine_validLine
  quickCheck prop_parseLine_invalidLine
  quickCheck prop_aggregateByCategory_correctTotals
  quickCheck prop_generateHTML_matchesSummary
