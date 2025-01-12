{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Components
  ( navigationBar
  ) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Database.Models (User(..))
import Database.Persist ( Entity (entityVal) )

navigationBar :: Maybe (Entity User) -> Html
navigationBar mUser = H.nav H.! A.class_ "navbar" $ do
  H.div H.! A.class_ "nav-content" $ do
    H.ul H.! A.class_ "nav-links" $ do
      H.li $ H.a H.! A.href "/" $ "Home"
      H.li $ H.a H.! A.href "/configuration" $ "Configuration"
    
    H.div H.! A.class_ "nav-user" $ do
      case mUser of
        Nothing -> H.a H.! A.href "/login" H.! A.class_ "btn-login" $ "Login"
        Just user -> do
          H.span H.! A.class_ "user-name" $ H.toHtml $ userEmail $ entityVal user
          H.a H.! A.href "/logout" H.! A.class_ "btn-logout" $ "Logout"