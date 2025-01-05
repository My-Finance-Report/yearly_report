{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.RefineSelectionPage (renderSliderPage) where

import Control.Monad (forM_)
import Data.Text as T (Text)
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types (Category (..), TransactionSource (..))

renderSliderPage ::
  Int ->
  T.Text ->
  [T.Text] ->
  [TransactionSource] ->
  TL.Text
renderSliderPage pdfId filename linesGuessed transactionSources =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "Select Start/End Keywords"

      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"

      H.script
        ! A.type_ "text/javascript"
        ! A.src "/updateKeywords.js"
        $ mempty

    H.body $ do
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
                forM_ transactionSources $ \TransactionSource {sourceId, sourceName} -> do
                  H.option
                    ! A.value (toValue sourceId)
                    $ toHtml sourceName

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
