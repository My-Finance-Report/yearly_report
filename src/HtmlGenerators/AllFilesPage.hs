
{-# LANGUAGE OverloadedStrings #-}
module HtmlGenerators.AllFilesPage (renderAllFilesPage) where

import Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


filenameListItem :: T.Text -> T.Text -> Html
filenameListItem baseUrl filename = H.li $ do
  let linkUrl = baseUrl <> filename
  H.a H.! A.href (toValue linkUrl) $ toHtml filename

renderAllFilesPage :: [T.Text] -> TL.Text
renderAllFilesPage filenames =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "All Files"
    H.body $ do
      H.h1 "All Files"
      H.ul $ do
        mapM_ (filenameListItem "/transactions/") filenames
