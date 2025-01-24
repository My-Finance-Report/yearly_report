{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( createUser,
    validateLogin,
    createSession,
    getTokenFromRequest,
    getCurrentUser,
    requireUser,
    validateSession,
    deleteSession,
  )
where

import qualified Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Word (Word8)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist
import Database.Persist.Postgresql
import System.Random (randomIO)
import Text.Printf (printf)
import Web.Scotty (ActionM, header, html, redirect)
import qualified Web.Scotty as Web

extractBearerToken :: TL.Text -> Maybe TL.Text
extractBearerToken header =
  let prefix = "Bearer "
   in if prefix `TL.isPrefixOf` header
        then Just $ TL.drop (TL.length prefix) header
        else Nothing

extractSessionCookie :: TL.Text -> Maybe TL.Text
extractSessionCookie cookies =
  let sessionPrefix = "session="
   in listToMaybe [TL.drop (TL.length sessionPrefix) cookie | cookie <- TL.splitOn "; " cookies, sessionPrefix `TL.isPrefixOf` cookie]

getCurrentUser :: ConnectionPool -> ActionM (Maybe (Entity User))
getCurrentUser pool = do
  mToken <- getTokenFromRequest
  case mToken of
    Nothing -> return Nothing
    Just token -> liftIO $ validateSession pool $ TL.toStrict token

hashPassword :: Text -> Text
hashPassword password =
  T.pack $ show (Hash.hash (encodeUtf8 password) :: Hash.Digest Hash.SHA256)

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

getTokenFromRequest :: ActionM (Maybe TL.Text)
getTokenFromRequest = do
  mCookie <- header "Cookie"
  case mCookie >>= extractSessionCookie of
    Just token -> return $ Just token
    Nothing -> do
      mAuthHeader <- header "Authorization"
      return $ mAuthHeader >>= extractBearerToken

requireUser :: ConnectionPool -> (Entity User -> ActionM ()) -> ActionM ()
requireUser pool action = do
  mToken <- getTokenFromRequest
  case mToken of
    Nothing -> redirect "/login"
    Just token -> do
      mUser <- liftIO $ validateSession pool $ TL.toStrict token
      case mUser of
        Nothing -> redirect "/login"
        Just user -> action user

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
