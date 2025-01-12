{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AllFilesPage (renderAllFilesPage) where

import Data.Text as T (Text, pack)
import Database.Models (UploadedPdf (uploadedPdfFilename))
import Database.Persist (Entity, entityKey, entityVal)
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
  ( Html,
    ToValue (toValue),
    a,
    body,
    h1,
    li,
    toHtml,
    ul,
    (!),
  )
import Text.Blaze.Html5.Attributes as A (href)

filenameListItem :: T.Text -> Entity UploadedPdf -> Html
filenameListItem baseUrl file = H.li $ do
  let linkUrl = baseUrl <> pack (show (fromSqlKey $ entityKey file))
  H.a H.! A.href (toValue linkUrl) $ toHtml $ uploadedPdfFilename (entityVal file)

renderAllFilesPage :: [Entity UploadedPdf] -> Html
renderAllFilesPage filenames =
  H.body $ do
    H.h1 "All Files"
    H.ul $ do
      mapM_ (filenameListItem "/transactions/") filenames
