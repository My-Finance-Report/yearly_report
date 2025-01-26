{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Components
  ( navigationBar,
    makeToolBar,
  )
where

import Database.Models (User (..))
import Database.Persist (Entity (entityVal))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

navigationBar :: Maybe (Entity User) -> Html
navigationBar mUser = H.nav H.! A.class_ "flex items-center justify-between bg-white border-b border-primary px-6 py-4 shadow-md" $ do
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

makeToolBar :: Html
makeToolBar =
  H.div ! A.class_ "flex flex-row items-center justify-center mt-4" $ do
    H.div ! A.class_ "flex flex-row gap-2 text-primary border-primary rounded-md border-[1px] p-4 bg-white shadow-sm" $ do
      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button"
        ! A.id "configureChartsButton"
        ! H.dataAttribute "path" "/new-configuration"
        ! A.onclick "window.location.href='/new-configuration'"
        $ "Configure Charts"

      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button"
        ! A.id "homeButton"
        ! H.dataAttribute "path" "/dashboard"
        ! A.onclick "window.location.href='/dashboard'"
        $ "Dashboard"

      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button"
        ! A.id "manageAccountsButton"
        ! H.dataAttribute "path" "/manage-accounts"
        ! A.onclick "window.location.href='/manage-accounts'"
        $ "Manage Accounts"

      H.button
        ! A.type_ "button"
        ! A.class_ "secondary-button"
        ! A.id "addTransactionsButton"
        ! H.dataAttribute "path" "/upload"
        ! A.onclick "window.location.href='/upload'"
        $ "Add Transactions"
