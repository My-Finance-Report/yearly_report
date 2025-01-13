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
    -- Add My Finance to the header
    H.div H.! A.class_ "nav-brand" $ do
      H.a H.! A.href "/" H.! A.class_ "brand-link" $ "My Finance"

    H.ul H.! A.class_ "nav-links" $ do
      H.li $ H.a H.! A.href "/dashboard" $ "Home"
      H.li $ H.a H.! A.href "/configuration" $ "Configuration"

    H.div H.! A.class_ "nav-user" $ do
      case mUser of
        Nothing -> H.a H.! A.href "/login" H.! A.class_ "btn-login" $ "Login"
        Just user -> do
          H.span H.! A.class_ "user-name" $ H.toHtml $ userEmail $ entityVal user
          H.a H.! A.href "/logout" H.! A.class_ "btn-logout" $ "Logout"