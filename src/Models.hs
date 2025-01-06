

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}


module Models where

import Database.Persist.TH
import Data.Text (Text)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

TransactionSource
    name Text
    deriving Show Eq

Category
    name Text
    sourceId TransactionSourceId
    UniqueCategory name sourceId
    deriving Show Eq

Transaction
    description Text
    categoryId CategoryId
    dateOfTransaction Text
    amount Double
    transactionSourceId TransactionSourceId
    kind Text
    filename Text Maybe
    deriving Show Eq

ProcessedFile
    filename Text
    UniqueProcessedFile filename
    deriving Show Eq

UploadedPdf
    filename Text
    rawContent Text
    uploadTime Text
    deriving Show Eq

UploadConfiguration
    filenameRegex Text Maybe
    startKeyword Text Maybe
    endKeyword Text Maybe
    transactionSourceId TransactionSourceId
    UniqueUploadConfiguration transactionSourceId
    deriving Show Eq

SankeyConfig
    name Text
    UniqueSankeyConfig name
    deriving Show Eq

SankeyInput
    configId SankeyConfigId
    sourceId TransactionSourceId
    categoryId CategoryId
    deriving Show Eq

SankeyLinkage
    configId SankeyConfigId
    sourceId TransactionSourceId
    categoryId CategoryId
    targetSourceId TransactionSourceId
    deriving Show Eq
|]
