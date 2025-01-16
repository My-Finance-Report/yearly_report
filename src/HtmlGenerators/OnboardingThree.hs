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


renderOnboardingThree :: Entity User -> [Entity TransactionSource] -> [UploadConfiguration] -> Html
renderOnboardingThree user transactionSources uploadConfigurations =
  H.body $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/css/onboarding.css"

    H.div ! A.class_ "page-header" $ do
      H.h1 "Onboarding"
      H.h2 "Step 3 of 3"
      H.h2 "Upload Example Files"
      H.p "Help us understand how to parse transactions for each of your account types by uploading an example file."
      H.h3 "You will only have to do this once, and you can do it later!"

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
        ! A.method "get"
        ! A.action "/onboarding/step-2"
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Previous"
            ! A.class_ "btn-next"

      H.form
        ! A.method "post"
        ! A.action "/onboarding/step-3"
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Finish"
            ! A.class_ "btn-next"

