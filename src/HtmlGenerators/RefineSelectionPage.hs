{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.RefineSelectionPage (renderSliderPage) where

import Control.Monad (forM_)
import Data.Text as T (Text)
import Database.Persist
    ( PersistEntity(Key), Entity(entityKey, entityVal) )
import Database.Persist.Postgresql (fromSqlKey)
import HtmlGenerators.Components (navigationBar)
import Models ( UploadedPdf, TransactionSource(..) )
import Text.Blaze.Html5 as H
    ( Html,
      toHtml,
      body,
      br,
      div,
      form,
      h1,
      h2,
      input,
      label,
      option,
      p,
      pre,
      script,
      select,
      span,
      ToValue(toValue),
      (!) )
import Text.Blaze.Html5.Attributes as A
    ( action,
      class_,
      for,
      id,
      method,
      name,
      oninput,
      placeholder,
      src,
      type_,
      value )

renderSliderPage ::
  Key UploadedPdf ->
  T.Text ->
  [T.Text] ->
  [Entity TransactionSource] ->
    Html
renderSliderPage pdfId filename linesGuessed transactionSources =
    H.body $ do
      H.script
            ! A.type_ "text/javascript"
            ! A.src "/updateKeywords.js"
            $ mempty
      H.div ! A.id "selectionContainer" $ do
        H.h1 "Select Transaction Boundaries"
        H.p $ toHtml ("File: " <> filename)

        H.form
          ! A.method "post"
          ! A.action "/setup-upload/"
          $ do
            H.label ! A.for "transactionSource" $ "Transaction Source: "
            H.select
              ! A.name "transactionSourceId"
              ! A.id "transactionSourceId"
              $ do
                forM_ transactionSources $ \entity -> do
                  let TransactionSource {transactionSourceName} = entityVal entity
                  H.option
                    ! A.value (toValue $ show $ fromSqlKey $ entityKey entity)
                    $ toHtml transactionSourceName

            H.br

            H.label ! A.for "startKeyword" $ "Start Keyword: "
            H.input
              ! A.type_ "text"
              ! A.name "startKeyword"
              ! A.id "startKeyword"
              ! A.placeholder "Enter start keyword"
              ! A.oninput "updatePreview()"

            H.br

            H.label ! A.for "endKeyword" $ "End Keyword: "
            H.input
              ! A.type_ "text"
              ! A.name "endKeyword"
              ! A.id "endKeyword"
              ! A.placeholder "Enter end keyword"
              ! A.oninput "updatePreview()"

            H.br

            H.label ! A.for "filenameRegex" $ "Filename Pattern: "
            H.input
              ! A.type_ "text"
              ! A.name "filenamePattern"
              ! A.id "filenamePattern"
              ! A.placeholder "Enter filename pattern"

            H.br
            H.input ! A.type_ "submit" ! A.value "Submit"

        H.br
        H.br

        H.h2 "Selected Text Preview"
        H.div ! A.id "previewContainer" $ ""

      H.pre
        ! A.id "linesContainer"
        ! A.class_ "linesContainer"
        $ do
          forM_ (Prelude.zip [0 ..] linesGuessed) $ \(idx, txt) -> do
            H.span ! A.id (toValue $ "line-" <> show idx) $ toHtml txt
            toHtml ("\n" :: String)
