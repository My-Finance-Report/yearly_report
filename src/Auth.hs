{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( createUser,
    validateLogin,
    createSession,
    validateSession,
    deleteSession,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import qualified Data.Text.Lazy as TL
import Database.ConnectionPool (getConnectionPool)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Database.Persist
import Database.Persist.Postgresql
import Database.Models
import Data.Word (Word8)
import Text.Printf (printf)
import qualified Control.Monad
import System.Random (randomIO)

hashPassword :: Text -> Text
hashPassword password =
  T.pack $ show  (Hash.hash (encodeUtf8 password) :: Hash.Digest Hash.SHA256)

createUser :: (MonadUnliftIO m) => ConnectionPool -> Text -> Text -> m (Either Text (Entity User))
createUser pool email password = do
  runSqlPool queryCreateUser pool
  where
    queryCreateUser = do
      existing <- getBy $ UniqueUser email
      case existing of
        Just _ -> return $ Left "Email already exists" 
        Nothing -> do
          now <- liftIO getCurrentTime
          let onboardStep = Just 0
          let hashedPassword = hashPassword password
          userId <- insert $ User email hashedPassword now onboardStep
          maybeUser <- get userId
          case maybeUser of
            Nothing -> return $ Left "Failed to create user" 
            Just user -> return $ Right $ Entity userId user 

validateLogin :: (MonadUnliftIO m) => ConnectionPool -> Text -> Text -> m (Maybe (Entity User))
validateLogin pool email password = do
  let hashedPassword = hashPassword password
  runSqlPool (selectFirst [UserEmail ==. email, UserPasswordHash ==. hashedPassword] []) pool

generateSessionToken :: IO Text
generateSessionToken = do
  randomBytes <- Control.Monad.replicateM 32 randomIO
  return $ T.pack $ concatMap (printf "%02x") (randomBytes :: [Word8])

createSession :: (MonadUnliftIO m) => ConnectionPool -> Key User -> m Text
createSession pool userId = do
  token <- liftIO generateSessionToken
  expiresAt <- liftIO $ addUTCTime (60 * 60 * 24 * 7) <$> getCurrentTime 
  _ <- runSqlPool (insert $ UserSession userId token expiresAt) pool
  return token

validateSession :: (MonadUnliftIO m) => ConnectionPool -> Text -> m (Maybe (Entity User))
validateSession pool token = do
  now <- liftIO getCurrentTime
  runSqlPool (validateSessionQuery now) pool
  where
    validateSessionQuery currentTime = do
      maybeSession <- selectFirst [UserSessionSessionToken ==. token, UserSessionExpiresAt >. currentTime] []
      case maybeSession of
        Nothing -> return Nothing
        Just (Entity _ session) -> do
          maybeUser <- get (userSessionUserId session)
          case maybeUser of
            Nothing -> return Nothing
            Just user -> return $ Just $ Entity (userSessionUserId session) user

deleteSession :: (MonadUnliftIO m) => ConnectionPool -> Text -> m ()
deleteSession pool token =
  runSqlPool (deleteWhere [UserSessionSessionToken ==. token]) pool
