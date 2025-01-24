{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.UploadPage (renderUploadPage, renderSelectAccountPage) where

import Control.Monad (forM_)
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

makeToolBar :: Html
makeToolBar =
  H.div ! A.class_ "w-full max-w-3xl mx-auto text-center" $ do
    H.script ! A.type_ "text/javascript" ! A.src "/upload.js" $ mempty

    -- Page Title
    H.h1 ! A.class_ "text-4xl font-bold text-primary mb-4" $ "Upload Transactions"
    H.p ! A.class_ "text-gray-600 text-lg mb-6" $ "Drag and drop your bank statement PDFs or click to upload."

    -- Upload Form
    H.form
      ! A.action "/upload"
      ! A.method "post"
      ! A.enctype "multipart/form-data"
      ! A.class_ "flex flex-col items-center gap-4"
      $ do
        -- Upload Section (Dropzone)
        H.div
          ! A.class_ "bg-white min-w-96 border-2 border-dashed border-primary rounded-lg p-6 shadow-md flex flex-col items-center cursor-pointer hover:bg-gray-100 transition"
          ! A.id "dropzone"
          ! A.ondragover "handleDragOver(event)"
          ! A.ondrop "handleFileDrop(event)"
          ! A.onclick "triggerFileInput()"
          $ do
            -- Dropzone Instruction
            H.p ! A.class_ "text-gray-700 font-medium mb-2" $ "Click or Drag files here"

            -- File Input (Hidden)
            H.input
              ! A.type_ "file"
              ! A.name "pdfFiles"
              ! A.id "pdfFileInput"
              ! A.class_ "hidden"
              ! A.accept "application/pdf"
              ! A.multiple "multiple"
              ! A.onchange "updateFileList()"

            -- File Preview List
            H.ul ! A.id "fileList" ! A.class_ "text-gray-700 text-sm mt-2" $ ""

        -- Upload Button
        H.button
          ! A.type_ "submit"
          ! A.class_ "primary-button mt-4"
          ! A.id "uploadButton"
          ! A.disabled "disabled"
          $ "Upload Files"

renderUploadPage :: Entity User -> Html
renderUploadPage user =
  makeToolBar

renderSelectAccountPage :: [(Entity UploadedPdf, Maybe (Entity TransactionSource))] -> [Entity TransactionSource] -> Html
renderSelectAccountPage fileRecords transactionSources =
  H.div ! A.class_ "w-full max-w-3xl mx-auto text-center p-6" $ do
    -- Page Title
    H.h1 ! A.class_ "text-4xl font-bold text-primary mb-4" $ "Select a Transaction Source"
    H.p ! A.class_ "text-gray-600 text-lg mb-6" $ "Choose a transaction source for the uploaded files that need one."

    -- Selection Form
    H.form
      ! A.action "/assign-transaction-source"
      ! A.method "post"
      ! A.class_ "flex flex-col items-center gap-4"
      $ do
        -- Table Listing PDFs
        H.table ! A.class_ "w-full border-collapse border border-gray-300 rounded-lg shadow-md" $ do
          H.thead $ H.tr ! A.class_ "bg-gray-200" $ do
            H.th ! A.class_ "p-3 border" $ "File Name"
            H.th ! A.class_ "p-3 border" $ "Transaction Source"

          H.tbody $ forM_ fileRecords $ \(Entity pdfId pdf, maybeSource) -> do
            H.tr ! A.class_ "border" $ do
              -- File Name Column
              H.td ! A.class_ "p-3 border text-left" $ H.toHtml (uploadedPdfFilename pdf)

              -- Transaction Source Selection Column
              H.td ! A.class_ "p-3 border" $ case maybeSource of
                Just (Entity sourceId source) ->
                  -- Show the assigned source as read-only
                  H.p ! A.class_ "text-gray-600" $ H.toHtml (transactionSourceName source)
                Nothing ->
                  -- Show the dropdown for missing configs
                  H.select
                    ! A.name (H.toValue $ "source-" <> show (fromSqlKey pdfId))
                    ! A.class_ "border p-2 rounded w-full"
                    $ do
                      H.option ! A.value "" $ "Select a source"
                      forM_ transactionSources $ \(Entity sourceId source) -> do
                        H.option ! A.value (H.toValue (fromSqlKey sourceId)) $ H.toHtml (transactionSourceName source)

        -- Submit Button (Only enabled if there are missing configs)
        if any (\(_, maybeSource) -> maybeSource == Nothing) fileRecords
          then H.button ! A.type_ "submit" ! A.class_ "mt-4 primary-button" $ "Save Selection"
          else H.p ! A.class_ "text-gray-600 mt-4" $ "All files already have a transaction source assigned."