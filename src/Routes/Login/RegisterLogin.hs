{-# LANGUAGE OverloadedStrings #-}

module Routes.Login.RegisterLogin (registerLoginRoutes) where

import Auth
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (getCategoriesBySource)
import Database.Configurations (saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Models
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool)
import Database.TransactionSource
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.OnboardingFour (renderOnboardingFour)
import HtmlGenerators.OnboardingOne
import HtmlGenerators.OnboardingThree (renderOnboardingThree)
import HtmlGenerators.OnboardingTwo (renderOnboardingTwo)
import Web.Scotty (ActionM, ScottyM, formParam, get, html, post, redirect, setHeader)

registerLoginRoutes :: ConnectionPool -> ScottyM ()
registerLoginRoutes pool = do
  get "/login" $ do
    token <- getTokenFromRequest

    case token of
      Just _ -> redirect "/dashboard"
      Nothing -> html $ renderPage Nothing "Login" (renderLoginPage Nothing) False

  post "/login" $ do
    email <- formParam "email" :: Web.Scotty.ActionM Text
    password <- Web.Scotty.formParam "password" :: Web.Scotty.ActionM Text
    maybeUser <- liftIO $ validateLogin pool email password
    case maybeUser of
      Nothing -> html $ renderPage Nothing "Login" (renderLoginPage (Just "Invalid email or password")) False
      Just user -> do
        token <- liftIO $ createSession pool (entityKey user)
        setHeader "Set-Cookie" $ fromStrict $ "session=" <> token <> "; Path=/; HttpOnly"
        redirect "/dashboard"

  post "/register" $ do
    email <- Web.Scotty.formParam "email" :: Web.Scotty.ActionM Text
    password <- Web.Scotty.formParam "password" :: Web.Scotty.ActionM Text
    confirmPassword <- Web.Scotty.formParam "confirm-password" :: Web.Scotty.ActionM Text

    if password /= confirmPassword
      then html $ renderPage Nothing "Login" (renderLoginPage (Just "Passwords do not match")) False
      else do
        result <- liftIO $ createUser pool email password
        case result of
          Left err -> html $ renderPage Nothing "Login"  (renderLoginPage (Just err)) False
          Right user -> do
            token <- liftIO $ createSession pool (entityKey user)
            setHeader "Set-Cookie" $ fromStrict $ "session=" <> token <> "; Path=/; HttpOnly"
            redirect "/dashboard"

  get "/logout" $ do
    mToken <- getTokenFromRequest

    case mToken of
      Nothing -> redirect "/login"
      Just token -> do
        liftIO $ deleteSession pool $ toStrict token
        setHeader "Set-Cookie" "session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly"
        redirect "/login"