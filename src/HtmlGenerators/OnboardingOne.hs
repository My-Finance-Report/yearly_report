{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingOne (renderOnboardingOne, newSourceComponent) where

import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderPrefilledCard :: Text -> [Entity TransactionSource] -> Html
renderPrefilledCard name transactionSources =
  unless (name `elem` Prelude.map (transactionSourceName . entityVal) transactionSources) $
    H.div ! A.class_ "border border-primary rounded-md p-6 shadow-md w-full flex flex-col items-center" $ do
      H.h3 ! A.class_ "text-lg font-semibold text-primary mb-4" $ toHtml name
      H.form
        ! A.method "post"
        ! A.action "/add-transaction-source"
        ! A.class_ "flex gap-2"
        $ do
          H.input
            ! A.type_ "hidden"
            ! A.name "newSource"
            ! A.value (toValue name)
          H.input
            ! A.type_ "submit"
            ! A.value "Add"
            ! A.class_ "primary-button"

newSourceComponent :: [Entity TransactionSource] -> Bool -> Html
newSourceComponent transactionSources includeDefaults =
  H.div ! A.class_ "flex flex-col sm:flex-row flex-wrap gap-6 w-full max-w-4xl" $ do
    -- Existing Accounts
    forM_ transactionSources $ \source -> do
      let txnName = transactionSourceName $ entityVal source

      H.div ! A.class_ "border border-primary rounded-md p-6 shadow-md w-full sm:w-1/3 flex flex-col items-center" $ do
        H.h3 ! A.class_ "text-lg font-semibold text-primary mb-4" $ toHtml txnName
        H.form
          ! A.method "post"
          ! A.action "/remove-transaction-source"
          ! A.class_ "flex gap-2"
          $ do
            H.input
              ! A.type_ "hidden"
              ! A.name "newSource"
              ! A.value (toValue txnName)
            H.input
              ! A.type_ "submit"
              ! A.value "Remove"
              ! A.class_ "secondary-button"

    -- Prefilled Cards for Common Accounts
    when includeDefaults $ do
      renderPrefilledCard "Credit Card" transactionSources
      renderPrefilledCard "Debit Card" transactionSources
      renderPrefilledCard "Savings Account" transactionSources
      renderPrefilledCard "Checking Account" transactionSources

    -- Add Custom Account
    H.div ! A.class_ "border border-dashed border-gray-400 rounded-md p-6 shadow-md w-full flex flex-col items-center" $ do
      H.form
        ! A.method "post"
        ! A.action "/add-transaction-source"
        ! A.class_ "flex gap-2 w-full sm:w-1/2"
        $ do
          H.input
            ! A.type_ "text"
            ! A.name "newSource"
            ! A.placeholder "Account Name"
            ! A.class_ "border border-gray-300 rounded-md p-2 flex-1"
            ! A.required "required"
          H.input
            ! A.type_ "submit"
            ! A.value "Add"
            ! A.class_ "primary-button"

renderOnboardingOne :: Entity User -> [Entity TransactionSource] -> Bool -> Html
renderOnboardingOne user transactionSources isOnboarding =
  let nextUrl =
        if isOnboarding
          then "/onboarding/step-2"
          else "/add-account/step-2"
      method =
        if isOnboarding
          then "post"
          else "get"
   in H.div ! A.class_ "bg-gray-50 text-gray-900 min-h-screen flex flex-col items-center p-6" $ do
        -- Page Header
        H.div ! A.class_ "w-full max-w-3xl text-center mb-8" $ do
          when isOnboarding $ do
            H.h1 ! A.class_ "text-4xl font-bold text-primary" $ "Onboarding"
            H.h2 ! A.class_ "text-lg text-gray-700 mt-2" $ "Step 1 of 2"
          H.h2 ! A.class_ "text-xl font-semibold text-gray-900 mt-4" $ "Add Accounts"
          H.p ! A.class_ "text-gray-600 mt-2" $ "Think Savings Account, Checking Account, Credit Card, etc."

        -- Account List & Input
        newSourceComponent transactionSources isOnboarding

        -- Navigation Button
        H.div ! A.class_ "flex flex-col sm:flex-row gap-4 mt-12 justify-center" $ do
          H.form
            ! A.method method
            ! A.action nextUrl
            $ do
              H.input
                ! A.type_ "submit"
                ! A.value "Next"
                ! A.class_ "primary-button"
