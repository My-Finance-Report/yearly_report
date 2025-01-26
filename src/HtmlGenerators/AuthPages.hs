{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AuthPages
  ( renderLoginPage,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import HtmlGenerators.Layout
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderLoginPage :: Maybe Text -> Html
renderLoginPage errorMsg =
  
  H.div ! A.class_ "flex justify-center mt-2" $ do
    H.div ! A.class_ "max-w-md w-full bg-white shadow-md rounded-lg p-6" $ do
      H.div ! A.class_ "mb-6" $ do
        H.h1 ! A.class_ "text-2xl font-semibold text-center text-gray-900 mb-4" $ "Login"
        case errorMsg of
          Just msg -> H.div ! A.class_ "text-red-600 text-sm text-center mb-4" $ toHtml msg
          Nothing -> return ()
        H.form ! A.method "post" ! A.class_ "flex flex-col gap-4" ! A.action "/login" $ do
          H.div ! A.class_ "flex flex-col" $ do
            H.label ! A.for "login-email" ! A.class_ "text-gray-700 font-medium" $ "Email"
            H.input
              ! A.type_ "email"
              ! A.name "email"
              ! A.autocomplete "username"
              ! A.id "login-email"
              ! A.class_ "border border-gray-300 rounded-md p-2 focus:ring-2 focus:ring-primary focus:border-primary"

          H.div ! A.class_ "flex flex-col" $ do
            H.label ! A.for "login-password" ! A.class_ "text-gray-700 font-medium" $ "Password"
            H.input
              ! A.type_ "password"
              ! A.name "password"
              ! A.id "login-password"
              ! A.autocomplete "current-password"
              ! A.class_ "border border-gray-300 rounded-md p-2 focus:ring-2 focus:ring-primary focus:border-primary"

          H.button ! A.type_ "submit" ! A.class_ "primary-button w-full mt-2" $ "Login"

      -- Divider
      H.div ! A.class_ "relative flex items-center justify-center my-6" $ do
        H.span ! A.class_ "absolute bg-white px-2 text-gray-500 text-sm" $ "or"
        H.div ! A.class_ "w-full border-t border-gray-300" $ ""

      -- Registration Section
      H.div ! A.class_ "mt-6" $ do
        H.h1 ! A.class_ "text-2xl font-semibold text-center text-gray-900 mb-4" $ "Create Account"
        H.form ! A.method "post" ! A.class_ "flex flex-col gap-4" ! A.action "/register" $ do
          H.div ! A.class_ "flex flex-col" $ do
            H.label ! A.for "register-email" ! A.class_ "text-gray-700 font-medium" $ "Email"
            H.input
              ! A.type_ "email"
              ! A.name "email"
              ! A.autocomplete "new-username"
              ! A.id "register-email"
              ! A.class_ "border border-gray-300 rounded-md p-2 focus:ring-2 focus:ring-primary focus:border-primary"

          H.div ! A.class_ "flex flex-col" $ do
            H.label ! A.for "register-password" ! A.class_ "text-gray-700 font-medium" $ "Password"
            H.input
              ! A.type_ "password"
              ! A.name "password"
              ! A.id "register-password"
              ! A.autocomplete "new-password"
              ! A.class_ "border border-gray-300 rounded-md p-2 focus:ring-2 focus:ring-primary focus:border-primary"

          H.div ! A.class_ "flex flex-col" $ do
            H.label ! A.for "confirm-password" ! A.class_ "text-gray-700 font-medium" $ "Confirm Password"
            H.input
              ! A.type_ "password"
              ! A.name "confirm-password"
              ! A.id "confirm-password"
              ! A.autocomplete "new-password"
              ! A.class_ "border border-gray-300 rounded-md p-2 focus:ring-2 focus:ring-primary focus:border-primary"

          H.button ! A.type_ "submit" ! A.class_ "secondary-button w-full mt-2" $ "Create Account"
