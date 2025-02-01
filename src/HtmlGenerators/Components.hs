{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Components
  ( navigationBar,
    mobileNavigationBar,
makeAddTransactionsBanner,
    makeToolBar,
    makeDemoBanner,
    makeSimpleBanner,
  )
where

import Data.Text (Text)
import Database.Models (User (..))
import Database.Persist (Entity (entityVal))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

navigationBar :: Maybe (Entity User) -> Html
navigationBar mUser = H.nav H.! A.class_ "hidden md:flex flex-row justify-between items-center bg-white border-b border-primary px-6 py-4 shadow-md" $ do
  -- Brand / Logo
  H.div H.! A.class_ "text-primary font-semibold text-lg" $ do
    let brandLink = case mUser of
          Nothing -> "/"
          Just _ -> "/dashboard"
    H.a H.! A.href brandLink H.! A.class_ "ml-12 hover:opacity-80 transition" $ "My Financé"

  -- User Section (Login / Logout)
  H.div H.! A.class_ "flex items-center gap-4" $ do
    case mUser of
      Nothing -> do
        H.a H.! A.href "/login" H.! A.class_ "primary-button" $ "Login"
      Just user -> do
        H.span H.! A.class_ "text-primary font-medium" $ H.toHtml $ userEmail $ entityVal user
        H.a H.! A.href "/logout" H.! A.class_ "primary-button" $ "Logout"

    H.a H.! A.href "/help" H.! A.class_ "secondary-button" $ "Help me!"

mobileNavigationBar :: Maybe (Entity User) -> Html
mobileNavigationBar mUser = H.nav H.! A.class_ "bg-white border-b border-primary px-4 py-3 shadow-md md:px-6 md:py-4 md:hidden" $ do
  -- Wrapper div to handle flex layout and mobile menu toggling
  H.div H.! A.class_ "flex items-center justify-between" $ do
    -- Brand / Logo
    H.div H.! A.class_ "text-primary font-semibold text-lg" $ do
      let brandLink = case mUser of
            Nothing -> "/"
            Just _ -> "/dashboard"
      H.a H.! A.href brandLink H.! A.class_ "hover:opacity-80 transition" $ "My Financé"

    -- Mobile Menu Button (Hamburger Icon)
    H.button
      H.! A.class_ "md:hidden block text-primary focus:outline-none"
      H.! A.onclick "document.getElementById('mobile-menu').classList.toggle('hidden')"
      $ "☰" -- Unicode hamburger icon (better to replace with an actual icon)

  -- Mobile Menu (hidden by default, toggled with JavaScript)
  H.div H.! A.id "mobile-menu" H.! A.class_ "hidden md:hidden flex flex-col gap-4 mt-3" $ do
    userLinks mUser

-- Helper function to generate user-specific links
userLinks :: Maybe (Entity User) -> Html
userLinks mUser = do
  case mUser of
    Nothing -> do
      H.a H.! A.href "/login" H.! A.class_ "primary-button w-full md:w-auto text-center" $ "Login"
    Just user -> do
      H.span H.! A.class_ "text-primary font-medium" $ H.toHtml $ userEmail $ entityVal user
      H.a H.! A.href "/logout" H.! A.class_ "primary-button w-full md:w-auto text-center" $ "Logout"

  H.a H.! A.href "/help" H.! A.class_ "secondary-button w-full md:w-auto text-center" $ "Help me!"

makeToolBar :: Html
makeToolBar =
  H.div ! A.class_ "flex flex-wrap items-center justify-center mt-4 md:mt-6 px-4" $ do
    H.div ! A.class_ "flex flex-wrap gap-3 md:gap-2 text-primary border-primary rounded-md border-[1px] p-4 bg-white shadow-sm w-full md:w-auto" $ do
      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button py-3 px-4 w-full md:w-auto"
        ! A.id "homeButton"
        ! H.dataAttribute "path" "/dashboard"
        ! A.onclick "window.location.href='/dashboard'"
        $ "Dashboard"

      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button py-3 px-4 w-full md:w-auto"
        ! A.id "configureChartsButton"
        ! H.dataAttribute "path" "/new-configuration"
        ! A.onclick "window.location.href='/new-configuration'"
        $ "Configure Charts"

      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button py-3 px-4 w-full md:w-auto"
        ! A.id "manageAccountsButton"
        ! H.dataAttribute "path" "/manage-accounts"
        ! A.onclick "window.location.href='/manage-accounts'"
        $ "Manage Accounts"

      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button py-3 px-4 w-full md:w-auto"
        ! A.id "addTransactionsButton"
        ! H.dataAttribute "path" "/upload"
        ! A.onclick "window.location.href='/upload'"
        $ "Add Transactions"

makeSimpleBanner :: Text -> Html
makeSimpleBanner banner = H.div ! A.class_ "bg-yellow-500 text-black text-center p-3 rounded-md" $ toHtml banner

makeDemoBanner :: Html
makeDemoBanner =
  H.div ! A.class_ "bg-yellow-500 text-black text-center p-3 rounded-md" $ do
    H.span "You are in demo mode. "
    H.a ! A.href "/login" ! A.class_ "underline" $ "Sign up now"

makeAddTransactionsBanner :: Html
makeAddTransactionsBanner =
  H.div ! A.class_ "bg-yellow-500 text-black text-center p-3 rounded-md" $ do
    H.span "You need to "
    H.a ! A.href "/upload" ! A.class_ "underline" $ "add transactions"
    H.span "  to get started."