{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.UploadPage (renderUploadPage) where

import Database.Models
import Database.Persist
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

makeToolBar :: Html
makeToolBar =
  H.div ! A.class_ "page-header" $ do
    H.div ! A.class_ "upload-section" $ do
      H.form
        ! A.action "/upload"
        ! A.method "post"
        ! A.class_ "upload_form"
        ! A.enctype "multipart/form-data"
        $ do
          H.input
            ! A.type_ "file"
            ! A.name "pdfFile"
            ! A.accept "application/pdf"
            ! A.id "pdfFileInput"
          H.button
            ! A.type_ "submit"
            ! A.class_ "btn upload-btn"
            ! A.id "uploadButton"
            $ "Add Transactions"

renderUploadPage :: Entity User -> Html
renderUploadPage user =
  makeToolBar