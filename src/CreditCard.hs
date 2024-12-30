{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CreditCard
   ( CreditCardTransaction(..)
  , parseCreditCardFile
  ) where

import Data.Time (Day, parseTimeM, defaultTimeLocale, formatTime)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data CreditCardTransaction = CreditCardTransaction --  this is just convention to name these the same
  { transactionDate :: Day
  , merchantName :: Text
  , amount :: Double
  } deriving (Show, Eq, Ord, Generic) -- basically to allow things like mapping (Ord), printing (show), etc

instance FromJSON CreditCardTransaction


parseTransaction :: Text -> Maybe CreditCardTransaction
parseTransaction line =
  -- could also write as a let ... in, then remove the where

  --let split_line = T.splitOn "," line
  --in 
  case split_line of
    -- this unpacks the list into a 5 member tuple (basically asserting a pattern)
    (dateStr:_:merchant:amountStr:_) -> do
      date <- parseTimeM True defaultTimeLocale "%b %d" (T.unpack dateStr)
      amount <- readMaybe (T.unpack $ T.dropWhile (== '$') amountStr)
      Just $ CreditCardTransaction date merchant amount
    -- if we dont have five lines then we fall through to here,which return Nothing
    _ -> Nothing
  -- i think the where syntax is more "functional" then let .. in 
  where
    split_line = T.splitOn "," line


parseCreditCardFile ::  FilePath -> IO [CreditCardTransaction]
parseCreditCardFile filePath = do
  content <- TIO.readFile filePath -- IO syntax
  -- same $ for precendece as above
  return $ mapMaybe parseTransaction  (T.lines content )

-- Helper to safely read a value
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing


