{-# LANGUAGE OverloadedStrings #-}

module Bank
  ( 
   parseBankFile
  ) where

import qualified Data.Map as Map


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (trace)
import CreditCard (Transaction (..))


parseLine :: Text -> Maybe Transaction
parseLine line =
    case broken_up_line of
      (date:check_number:desc:deposits:withdrawals:balance:_) -> do
        let deposit     = parseAmount deposits   
            withdraw    = parseAmount withdrawals

        -- If there is a withdrawal, make it negative;
        -- If there is a deposit, keep it positive.
        -- If both or neither are present, fail with Nothing.
        finalAmount <- case (withdraw, deposit) of
                         (Just w, Nothing) -> Just (-w)
                         (Nothing, Just d) -> Just d
                         _                 -> Nothing

        Just (Transaction date desc finalAmount)

      -- If the line does not have at least 7 CSV parts, fail:
      _ -> Nothing
  where
    broken_up_line = T.splitOn "," line


-- Helper to parse an amount from text
parseAmount :: Text -> Maybe Double
parseAmount t =
    if T.null t || not (T.any (`elem` ("0123456789." :: String)) t)
        then Nothing
        else case reads (T.unpack t) of
        [(n, "")] -> Just n
        _         -> Nothing

parseBankFile :: FilePath -> IO [Transaction]
parseBankFile filePath = do
  content <- TIO.readFile filePath
  let lines = T.lines content
  return $ mapMaybe parseLine lines
