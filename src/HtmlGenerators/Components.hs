{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Components
  ( navigationBar,
  )
where

import Database.Models (User (..))
import Database.Persist (Entity (entityVal))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

navigationBar :: Maybe (Entity User) -> Html
navigationBar mUser = H.nav H.! A.class_ "navbar" $ do
  H.div H.! A.class_ "nav-content" $ do
    H.div H.! A.class_ "nav-brand" $ do
      case mUser of
        Nothing -> H.a H.! A.href "/" H.! A.class_ "brand-link" $ "My Financé"
        Just user -> H.a H.! A.href "/dashboard" H.! A.class_ "brand-link" $ "My Financé"

    H.ul H.! A.class_ "nav-links" $ do
      H.li $ H.a H.! A.href "/dashboard" $ "Home"
      H.li $ H.a H.! A.href "/help" $ "Help Me!"

    H.div H.! A.class_ "nav-user" $ do
      case mUser of
        Nothing -> H.a H.! A.href "/login" H.! A.class_ "btn-login" $ "Login"
        Just user -> do
          H.span H.! A.class_ "user-name" $ H.toHtml $ userEmail $ entityVal user
          H.a H.! A.href "/logout" H.! A.class_ "btn-logout" $ "Logout"
