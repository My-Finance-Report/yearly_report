{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.UploadPage (renderUploadPage) where

import Database.Models
import Database.Persist
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