{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Models where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics

data TransactionKind = Withdrawal | Deposit
  deriving (Show, Eq, Ord, Generic)

instance PersistField TransactionKind where
  toPersistValue Withdrawal = PersistText "Withdrawal"
  toPersistValue Deposit = PersistText "Deposit"

  fromPersistValue (PersistText "Withdrawal") = Right Withdrawal
  fromPersistValue (PersistText "Deposit") = Right Deposit
  fromPersistValue _ = Left "Invalid TransactionKind"

instance PersistFieldSql TransactionKind where
  sqlType _ = SqlString

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

TransactionSource
    name Text
    userId UserId 
    archived Bool default=False
    UniqueTransactionSource userId name
    deriving Show Eq Ord

Category
    name Text
    sourceId TransactionSourceId
    userId UserId 
    archived Bool default=False
    UniqueCategory name sourceId
    deriving Show Eq Ord

Transaction
    description Text
    categoryId CategoryId
    dateOfTransaction UTCTime
    amount Double
    transactionSourceId TransactionSourceId
    kind TransactionKind
    uploadedPdfId UploadedPdfId Maybe 
    userId UserId 
    archived Bool default=False
    deriving Show Eq Generic Ord

ProcessedFile
    filename Text
    userId UserId
    UniqueProcessedFile filename userId
    deriving Show Eq

UploadedPdf
    filename Text
    rawContent Text
    uploadTime Text
    userId UserId
    deriving Show Eq

UploadConfiguration
    filenameRegex Text Maybe
    startKeyword Text Maybe
    endKeyword Text Maybe
    transactionSourceId TransactionSourceId
    UniqueUploadConfiguration transactionSourceId
    userId UserId 
    deriving Show Eq

SankeyConfig
    name Text
    userId UserId 
    deriving Show Eq

SankeyInput
    configId SankeyConfigId
    sourceId TransactionSourceId -- DEPRECATED: We don't need this anymore
    categoryId CategoryId
    deriving Show Eq

SankeyLinkage
    configId SankeyConfigId
    sourceId TransactionSourceId
    categoryId CategoryId
    targetSourceId TransactionSourceId
    deriving Show Eq

User
    email Text
    passwordHash Text
    createdAt UTCTime
    onboardingStep Int Maybe
    UniqueUser email
    deriving Show Eq

UserSession
    userId UserId
    sessionToken Text
    expiresAt UTCTime
    UniqueUserSession sessionToken
    deriving Show Eq
|]
