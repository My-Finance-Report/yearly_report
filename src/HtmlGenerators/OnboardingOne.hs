{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingOne (renderOnboardingOne, newSourceComponent) where

import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

newSourceComponent :: [Entity TransactionSource] -> Bool -> Html
newSourceComponent transactionSources includeDefaults = do
  let filterByKind kind = filter (\(Entity _ source) -> transactionSourceSourceKind source == kind) transactionSources

  let accountSources = filterByKind Account
  let cardSources = filterByKind Card
  let investmentSources = filterByKind Investment

  H.div ! A.class_ "flex flex-col items-center gap-6  max-w-4xl" $ do
    -- Section: Accounts (Savings, Checking)
    renderSourceGroup "Accounts" accountSources Account includeDefaults
    -- Section: Cards (Credit, Debit)
    renderSourceGroup "Cards" cardSources Card includeDefaults
    -- Section: Investments (if applicable)
    renderSourceGroup "Investments" investmentSources Investment includeDefaults

renderSourceGroup :: Text -> [Entity TransactionSource] -> SourceKind -> Bool -> Html
renderSourceGroup title sources kind includeDefaults = do
  H.div ! A.class_ "" $ do
    H.h3 ! A.class_ "text-2xl font-semibold text-primary mb-2" $ toHtml title

    -- Render Existing Sources
    unless (null sources) $
      H.div ! A.class_ "flex flex-col gap-2" $ do
        forM_ sources $ \(Entity sourceId source) -> do
          H.div ! A.class_ "min-w-[600px] flex items-center gap-2 bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all focus:outline-none" $ do
            -- Edit Form
            H.form
              ! A.method "post"
              ! A.action (toValue $ "/edit-transaction-source/" <> show (fromSqlKey sourceId))
              ! A.class_ "flex flex-1 items-center gap-2"
              $ do
                -- Editable Input Field for Name
                H.input
                  ! A.type_ "text"
                  ! A.name "updatedSourceName"
                  ! A.value (toValue $ transactionSourceName source)
                  ! A.class_ "min-w-96 border border-gray-300 rounded-md p-2 flex-1"

                -- Hidden Field for Source Kind
                H.input
                  ! A.type_ "hidden"
                  ! A.name "sourceKind"
                  ! A.value (toValue $ show kind)

                -- Save Button
                H.input
                  ! A.type_ "submit"
                  ! A.value "Update"
                  ! A.class_ "secondary-button"

            -- Remove Form (Separate Form)
            H.form
              ! A.method "post"
              ! A.action "/remove-transaction-source"
              ! A.class_ "flex items-center"
              $ do
                -- Hidden Fields to Identify Source
                H.input
                  ! A.type_ "hidden"
                  ! A.name "sourceName"
                  ! A.value (toValue $ transactionSourceName source)
                H.input
                  ! A.type_ "hidden"
                  ! A.name "sourceKind"
                  ! A.value (toValue $ show kind)

                -- Remove Button
                H.input
                  ! A.type_ "submit"
                  ! A.value "Remove"
                  ! A.class_ "secondary-danger-button"

    -- Render Prefilled Options if IncludeDefaults is True
    when includeDefaults $ do
      case kind of
        Account -> do
          renderPrefilledCard "Savings Account" Account sources
          renderPrefilledCard "Checking Account" Account sources
          renderCustomAccountForm Account
        Card -> do
          renderPrefilledCard "Credit Card" Card sources
          renderPrefilledCard "Debit Card" Card sources
          renderCustomAccountForm Card
        Investment -> do
          renderPrefilledCard "Stock Portfolio" Investment sources
          renderPrefilledCard "Retirement Fund" Investment sources
          renderCustomAccountForm Investment

renderPrefilledCard :: Text -> SourceKind -> [Entity TransactionSource] -> Html
renderPrefilledCard name kind transactionSources =
  unless (name `elem` Prelude.map (transactionSourceName . entityVal) transactionSources)
    $ H.form
      ! A.method "post"
      ! A.action "/add-transaction-source"
      ! A.class_ "min-w-[600px] flex items-center gap-2 bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all focus:outline-none cursor-pointer"
    $ do
      H.input
        ! A.type_ "text"
        ! A.name "newSource"
        ! A.value (toValue name)
        ! A.class_ "min-w-96 border border-gray-300 rounded-md p-2 flex-1"

      H.input
        ! A.type_ "hidden"
        ! A.name "newKind"
        ! A.value
          (toValue $ show kind) -- Include the kind in the form
      H.input
        ! A.type_ "submit"
        ! A.value "Add"
        ! A.class_ "primary-button"

renderCustomAccountForm :: SourceKind -> Html
renderCustomAccountForm kind = do
  H.form
    ! A.method "post"
    ! A.action "/add-transaction-source"
    ! A.class_ "flex items-center gap-2 bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all focus:outline-none cursor-pointer"
  $ do
    H.input
      ! A.type_ "text"
      ! A.name "newSource"
      ! A.class_ "min-w-96 border border-gray-300 rounded-md p-2 flex-1"

    H.input
      ! A.type_ "hidden"
      ! A.name "newKind"
      ! A.value
        (toValue $ show kind) -- Include the kind in the form
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
