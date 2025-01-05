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
      H.title "Select Start/End with Sliders"

      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"

      H.script
        ! A.type_ "text/javascript"
        ! A.src "/slider.js"
        $ mempty

    H.body $ do
      H.div ! A.id "slidersContainer" $ do
        H.h1 "Select Transaction Boundaries"
        H.p $ toHtml ("File: " <> filename)

        H.label ! A.for "startLine" $ "Start line: "
        H.input
          ! A.type_ "range"
          ! A.id "startLine"
          ! A.name "startLine"
          ! A.min "0"
          ! A.max (toValue (Prelude.length linesGuessed - 1))
          ! A.value "0"
          ! A.oninput "updateRange()"

        H.br

        H.label ! A.for "endLine" $ "End line: "
        H.input
          ! A.type_ "range"
          ! A.id "endLine"
          ! A.name "endLine"
          ! A.min "0"
          ! A.max (toValue (Prelude.length linesGuessed - 1))
          ! A.value (toValue (Prelude.length linesGuessed - 1))
          ! A.oninput "updateRange()"

        H.br

        H.form
          ! A.method "post"
          ! A.action (toValue ("/confirm-boundaries/" <> show pdfId))
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

            H.label ! A.for "filenameRegex" $ "Filename Pattern: "
            H.input
              ! A.type_ "text"
              ! A.name "filenameRegex"
              ! A.id "filenameRegexId"
              ! A.placeholder "Enter filename regex"

            H.input ! A.type_ "hidden" ! A.name "finalStart" ! A.id "finalStartId"
            H.input ! A.type_ "hidden" ! A.name "finalEnd" ! A.id "finalEndId"
            H.br
            H.input ! A.type_ "submit" ! A.value "Submit"

      H.br
      H.br

      H.pre
        ! A.id "linesContainer"
        ! A.class_ "linesContainer"
        $ do
          forM_ (Prelude.zip [0 ..] linesGuessed) $ \(idx, txt) -> do
            H.span ! A.id (toValue $ "line-" <> show idx) $ toHtml txt
            toHtml ("\n" :: String)
