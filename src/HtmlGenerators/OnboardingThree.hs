{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingThree (renderOnboardingThree) where

import Control.Monad (forM_, when)
import Data.Map (Map, toList)
import Data.Text (Text, pack)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderOnboardingThree :: Entity User -> [Entity TransactionSource] -> [UploadConfiguration] -> Bool -> Html
renderOnboardingThree user transactionSources uploadConfigurations isOnboarding =
  let nextUrl =
        if isOnboarding
          then "/onboarding/step-4"
          else "/dashboard"
      prevUrl =
        if isOnboarding
          then "/onboarding/step-2"
          else "/add-account/step-2"
      method =
        if isOnboarding
          then "post"
          else "get"
      allCompleted = all (\(Entity sourceId _) -> any (\config -> uploadConfigurationTransactionSourceId config == sourceId) uploadConfigurations) transactionSources
   in H.div ! A.class_ "bg-gray-50 text-gray-900 min-h-screen flex flex-col items-center p-6" $ do
        -- Header
        H.div ! A.class_ "w-full max-w-3xl text-center mb-8" $ do
          when isOnboarding $ do
            H.h1 ! A.class_ "text-4xl font-bold text-primary" $ "Onboarding"
            H.h2 ! A.class_ "text-lg text-gray-700 mt-2" $ "Step 3 of 3"
          H.h2 ! A.class_ "text-xl font-semibold text-gray-900 mt-4" $ "Upload Example Files"
          H.p ! A.class_ "text-gray-600 mt-2" $ "We use this file to determine how to process future files for this account"

        -- Upload Section
        renderTransactionSources transactionSources uploadConfigurations

        -- Buttons
        H.div ! A.class_ "flex flex-col sm:flex-row gap-4 mt-12 justify-center" $ do
          -- Previous Button
          H.form
            ! A.method method
            ! A.action prevUrl
            $ do
              H.input
                ! A.type_ "submit"
                ! A.value "Previous"
                ! A.class_ "secondary-button"

          -- Finish Button (Initially Disabled)
          H.form
            ! A.method method
            ! A.action nextUrl
            ! A.onsubmit "markProcessing(this)"
            $ do
              H.input
                ! A.type_ "submit"
                ! A.value "Finish"
                ! A.id "finish-button"
                ! A.class_ "primary-button disabled:opacity-50"
                !? (not allCompleted, A.disabled "disabled")

renderTransactionSources :: [Entity TransactionSource] -> [UploadConfiguration] -> Html
renderTransactionSources transactionSources uploadConfigurations =
  H.div ! A.class_ "flex flex-col sm:flex-row gap-6" $ do
    forM_ transactionSources $ \(Entity sourceId source) -> do
      let isCompleted = any (\config -> uploadConfigurationTransactionSourceId config == sourceId) uploadConfigurations
      uploadConfigCard sourceId source isCompleted

-- Reusable Upload Config Card
uploadConfigCard :: Key TransactionSource -> TransactionSource -> Bool -> Html
uploadConfigCard sourceId source isCompleted =
  H.div ! A.class_ "flex flex-col items-center border border-primary rounded-md p-6 shadow-md w-full" $ do
    -- Section Title
    H.h3 ! A.class_ "text-lg font-semibold text-primary mb-4" $ toHtml $ transactionSourceName source

    -- Upload Section
    H.div ! A.class_ "upload-section" $ do
      if isCompleted
        then H.p ! A.class_ "text-green-600 font-medium" $ "Completed"
        else H.form
          ! A.method "post"
          ! A.enctype "multipart/form-data"
          ! A.class_ "flex flex-col gap-4"
          ! A.action (toValue $ "/upload-example-file/" <> show (fromSqlKey sourceId))
          ! A.onsubmit "markProcessing(this)"
          $ do
            H.input
              ! A.type_ "file"
              ! A.name "exampleFile"
              ! A.class_ "file-input border border-gray-300 rounded-md p-2"
              ! A.accept "application/pdf"
            H.input
              ! A.type_ "submit"
              ! A.value "Upload File"
              ! A.class_ "primary-button"
