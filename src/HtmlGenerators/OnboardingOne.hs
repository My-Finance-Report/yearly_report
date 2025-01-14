{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingOne (renderOnboardingOne) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Control.Monad (forM_)

renderOnboardingOne :: Entity User -> [Entity TransactionSource] -> Html
renderOnboardingOne user transactionSources =
  H.body $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/css/onboarding.css"

    H.div ! A.class_ "page-header" $ do
      H.h1 "Onboarding"
      H.h2 "Step 1 of 3"
      H.h2 "Add an account type"
      H.p "Each account will have its transactions grouped together. Think Saving Account, Checking Account, Credit Card, etc."

    H.div ! A.class_ "card-container" $ do

      forM_ transactionSources $ \source -> do
        let txnName = transactionSourceName $ entityVal source
        H.div ! A.class_ "card" $ do
          H.h3 $ toHtml txnName
          H.p "This account type has been added."

      renderPrefilledCard "Credit Card" transactionSources
      renderPrefilledCard "Debit Card" transactionSources
      renderPrefilledCard "Savings Account" transactionSources
      renderPrefilledCard "Checking Account" transactionSources

      H.div ! A.class_ "card" $ do
        H.h3 "Custom Account Type"
        H.form
          ! A.method "post"
          ! A.action "/add-transaction-source"
          $ do
            H.div ! A.class_ "form-group" $ do
              H.input
                ! A.type_ "text"
                ! A.name "newSource"
                ! A.placeholder "Enter a custom account name"
            H.br
            H.input
              ! A.type_ "submit"
              ! A.value "Add Source"
              ! A.class_ "btn-submit"

    H.div ! A.class_ "next-button-container" $ do
        H.form
          ! A.method "get"
          ! A.action "/onboarding/step-2"
          $ do
            H.input
              ! A.type_ "submit"
              ! A.value "Next"
              ! A.class_ "btn-next"

renderPrefilledCard :: Text -> [Entity TransactionSource] -> Html
renderPrefilledCard name transactionSources =
  if name `elem` Prelude.map (transactionSourceName . entityVal) transactionSources
    then return () 
    else
      H.div ! A.class_ "card" $ do
        H.h3 $ toHtml name
        H.form
          ! A.method "post"
          ! A.action "/add-transaction-source"
          $ do
            H.div ! A.class_ "form-group" $ do
              H.input
                ! A.type_ "hidden" 
                ! A.name "newSource"
                ! A.value (toValue name)
            H.input
              ! A.type_ "submit"
              ! A.value "Save"
              ! A.class_ "btn-submit"


