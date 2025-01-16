{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingThree (renderOnboardingThree) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import Data.Map (Map, toList)
import Database.Models
import Database.Persist.Postgresql (fromSqlKey)
import Database.Persist (Entity (..))
import Control.Monad (forM_)

  
renderOnboardingThree :: Entity User -> [Entity TransactionSource] -> [UploadConfiguration] -> Bool-> Html
renderOnboardingThree user transactionSources uploadConfigurations isOnboarding=
  let nextUrl = if isOnboarding
                  then "/onboarding/step-3"
                  else "/dashboard"
      prevUrl = if isOnboarding
                  then "/onboarding/step-2"
                  else "/add-account/step-2"
      method = if isOnboarding
                  then "post"
                  else "get"

  in H.body $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/css/onboarding.css"

    H.div ! A.class_ "page-header" $ do
      case isOnboarding of
        True -> do
              H.h1 "Onboarding"
              H.h2 "Step 3 of 3"
        False -> return ()
      H.h2 "Upload Example File"
      H.p "We use this file to determine how to process future files for this account"

    H.div ! A.class_ "config-columns" $ do
      forM_ transactionSources $ \(Entity sourceId source) -> do
        let isCompleted = any (\config -> uploadConfigurationTransactionSourceId config == sourceId) uploadConfigurations

        H.div ! A.class_ "config-column" $ do
          H.h3 $ toHtml $ transactionSourceName source

          H.div ! A.class_ "upload-section" $ do
            if isCompleted
              then H.p "Completed" -- Display "Completed" if configuration exists
              else H.form
                ! A.method "post"
                ! A.enctype "multipart/form-data"
                ! A.class_ "blah"
                ! A.action (toValue $ "/upload-example-file/" <> show (fromSqlKey sourceId))
                $ do
                    H.input
                      ! A.type_ "file"
                      ! A.name "exampleFile"
                      ! A.id "exampleFile"
                      ! A.accept "application/pdf"
                    H.input
                      ! A.type_ "submit"
                      ! A.value "Upload File"
                      ! A.class_ "btn-submit"

    H.div ! A.class_ "button-container" $ do
      H.form
        ! A.method method
        ! A.action prevUrl
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Previous"
            ! A.class_ "btn-next"

      H.form
        ! A.method method
        ! A.action nextUrl
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Finish"
            ! A.class_ "btn-next"

