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

data JobStatus = Completed | Processing | Failed | Retrying | Pending
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

instance PersistField JobStatus where
  toPersistValue Completed = PersistText "Completed"
  toPersistValue Pending = PersistText "Pending"
  toPersistValue Processing = PersistText "Processing"
  toPersistValue Failed = PersistText "Failed"
  toPersistValue Retrying = PersistText "Retrying"

  fromPersistValue (PersistText "Completed") = Right Completed
  fromPersistValue (PersistText "Processing") = Right Processing
  fromPersistValue (PersistText "Pending") = Right Pending
  fromPersistValue (PersistText "Failed") = Right Failed
  fromPersistValue (PersistText "Retrying") = Right Retrying
  fromPersistValue _ = Left "Invalid TransactionStatus"

instance PersistFieldSql JobStatus where
  sqlType _ = SqlString

data JobKind = ParseTransactions | CategorizeTransactions | GenerateSankeyConfig
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

instance PersistField JobKind where
  toPersistValue ParseTransactions = PersistText "ParseTransactions"
  toPersistValue CategorizeTransactions = PersistText "CategorizeTransactions"
  toPersistValue GenerateSankeyConfig = PersistText "GenerateSankeyConfig"

  fromPersistValue (PersistText "ParseTransactions") = Right ParseTransactions
  fromPersistValue (PersistText "CategorizeTransactions") = Right CategorizeTransactions
  fromPersistValue (PersistText "GenerateSankeyConfig") = Right GenerateSankeyConfig
  fromPersistValue _ = Left "Invalid JobKind"

instance PersistFieldSql JobKind where
  sqlType _ = SqlString

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

data SourceKind = Investment | Account | Card
  deriving (Show, Eq, Ord, Generic)

instance PersistField SourceKind where
  toPersistValue Investment = PersistText "Investment"
  toPersistValue Account = PersistText "Account"
  toPersistValue Card = PersistText "Card"

  fromPersistValue (PersistText "Investment") = Right Investment
  fromPersistValue (PersistText "Account") = Right Account
  fromPersistValue (PersistText "Card") = Right Card
  fromPersistValue _ = Left "Invalid SourceKind"

parseSourceKind :: Text -> Maybe SourceKind
parseSourceKind "Investment" = Just Investment
parseSourceKind "Account" = Just Account
parseSourceKind "Card" = Just Card
parseSourceKind other = Nothing

instance PersistFieldSql SourceKind where
  sqlType _ = SqlString

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

TransactionSource
    name Text
    userId UserId 
    archived Bool default=False
    sourceKind SourceKind default='Account'
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

-- this shouldnt be used, use ProcessedFileJob instead
DeprecatedProcessedFile
    filename Text
    userId UserId
    uploadConfigurationId UploadConfigurationId Maybe -- todo remove this maybe
    uploadedPdfId UploadedPdfId Maybe --todo remove this maybe
    status JobStatus default='Completed'
    DeprecatedUniqueProcessedFile filename userId
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

ProcessFileJob
   createdAt UTCTime
   lastTriedAt UTCTime Maybe
   status JobStatus
   userId UserId
   configId UploadConfigurationId
   pdfId  UploadedPdfId
   attemptCount Int default=0
|]
