{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Layout (renderPage) where

import qualified Data.Text.Lazy as TL
import Database.Models
import Database.Persist (Entity)
import HtmlGenerators.Components (navigationBar)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderPage :: Maybe (Entity User) -> TL.Text -> Html -> TL.Text
renderPage mUser pageTitle content =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title $ toHtml pageTitle
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/css/navbar.css"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/css/landing.css"

      H.script ! A.type_ "text/javascript" ! A.src "/allPages.js" $ mempty

      H.meta ! A.property "og:title" ! A.content "My Financé - Simple Personal Finance"
      H.meta ! A.property "og:description" ! A.content "Because you should love your finances. Simple personal finance for just $2/month or $20/year."
      H.meta ! A.property "og:image" ! A.content "/landing.png"
      H.meta ! A.property "og:url" ! A.content "https://myfinancereport.com"
      H.meta ! A.property "og:type" ! A.content "website"

    H.body $ do
      navigationBar mUser
      H.div ! A.class_ "page-content" $ content
