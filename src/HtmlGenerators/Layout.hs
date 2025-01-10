{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Layout (renderPage) where

import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import HtmlGenerators.Components (navigationBar)
import Models
import Database.Persist (Entity)

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
    H.body $ do
      navigationBar mUser
      H.div ! A.class_ "page-content" $ content
