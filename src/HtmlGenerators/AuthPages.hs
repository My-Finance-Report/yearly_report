{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AuthPages
  ( renderLoginPage,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import HtmlGenerators.Layout


renderLoginPage :: Maybe Text -> Html
renderLoginPage errorMsg = 
  H.body $ do
    H.div ! A.class_ "auth-container" $ do
      H.div ! A.class_ "auth-section" $ do
        H.h1 "Login"
        case errorMsg of
          Just msg -> H.div ! A.class_ "error-message" $ toHtml msg
          Nothing -> return ()
        H.form ! A.method "post" ! A.action "/login" $ do
          H.div ! A.class_ "form-group" $ do
            H.label ! A.for "login-email" $ "Email"
            H.input ! A.type_ "email" ! A.name "email" ! A.id "login-email" ! A.class_ "form-control"
          H.div ! A.class_ "form-group" $ do
            H.label ! A.for "login-password" $ "Password"
            H.input ! A.type_ "password" ! A.name "password" ! A.id "login-password" ! A.class_ "form-control"
          H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Login"
      H.div ! A.class_ "auth-divider" $ do
        H.span ""
      H.div ! A.class_ "auth-section" $ do
        H.h1 "Create Account"
        H.form ! A.method "post" ! A.action "/register" $ do
          H.div ! A.class_ "form-group" $ do
            H.label ! A.for "register-email" $ "Email"
            H.input ! A.type_ "email" ! A.name "email" ! A.id "register-email" ! A.class_ "form-control"
          H.div ! A.class_ "form-group" $ do
            H.label ! A.for "register-password" $ "Password"
            H.input ! A.type_ "password" ! A.name "password" ! A.id "register-password" ! A.class_ "form-control"
          H.div ! A.class_ "form-group" $ do
            H.label ! A.for "confirm-password" $ "Confirm Password"
            H.input ! A.type_ "password" ! A.name "confirm-password" ! A.id "confirm-password" ! A.class_ "form-control"
          H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Create Account"
