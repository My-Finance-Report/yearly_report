{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AllFilesPage (renderAllFilesPage) where

import Data.Text as T ( Text )
import Text.Blaze.Html5 as H
    ( Html, toHtml, a, body, h1, li, ul, ToValue(toValue), (!) )
import Text.Blaze.Html5.Attributes as A ( href )

filenameListItem :: T.Text -> T.Text -> Html
filenameListItem baseUrl filename = H.li $ do
  let linkUrl = baseUrl <> filename
  H.a H.! A.href (toValue linkUrl) $ toHtml filename

renderAllFilesPage :: [T.Text] -> Html
renderAllFilesPage filenames =
    H.body $ do
      H.h1 "All Files"
      H.ul $ do
        mapM_ (filenameListItem "/transactions/") filenames
