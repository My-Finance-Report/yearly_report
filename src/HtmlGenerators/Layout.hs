{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Layout (renderPage) where

import Control.Monad (when)
import qualified Data.Text.Lazy as TL
import Database.Models
import Database.Persist (Entity)
import HtmlGenerators.Components (makeToolBar, mobileNavigationBar, navigationBar)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderPage :: Maybe (Entity User) -> TL.Text -> Html -> Bool -> TL.Text
renderPage mUser pageTitle content showToolbar =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title $ toHtml pageTitle
      H.link
        ! A.rel "stylesheet"
        ! A.href "/css/output.css"

      H.script ! A.type_ "text/javascript" ! A.src "/allPages.js" $ mempty

      -- Mobile-friendly meta viewport
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"

      -- OpenGraph metadata
      H.meta ! A.property "og:title" ! A.content "My Financé - Simple Personal Finance"
      H.meta ! A.property "og:description" ! A.content "Because you should love your finances. Simple personal finance for just $2/month or $20/year."
      H.meta ! A.property "og:image" ! A.content "/landing.png"
      H.meta ! A.property "og:url" ! A.content "https://myfinancereport.com"
      H.meta ! A.property "og:type" ! A.content "website"

    H.body ! A.class_ "bg-gray-50 text-gray-900 font-sans" $ do
      -- Navigation bars
      navigationBar mUser
      mobileNavigationBar mUser

      -- Optional toolbar
      when showToolbar makeToolBar

      -- Responsive content container
      H.div ! A.class_ "max-w-screen-2xl mx-auto px-4 py-6 text-lg sm:text-base leading-relaxed" $ do
        content
