{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingTwo (renderOnboardingTwo) where

import Control.Monad (forM_, unless, when)
import Data.Map (Map, toList)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderOnboardingTwo :: Entity User -> Map (Entity TransactionSource) [Entity Category] -> Bool -> Html
renderOnboardingTwo user transactions isOnboarding =
  let nextUrl =
        if isOnboarding
          then "/onboarding/finalize"
          else "/add-account/step-3"
      prevUrl =
        if isOnboarding
          then "/onboarding/step-1"
          else "/add-account/step-1"
      method =
        if isOnboarding
          then "post"
          else "get"
   in H.div ! A.class_ "bg-gray-50 text-gray-900 min-h-screen flex flex-col items-center p-6" $ do
        -- Page Header
        H.div ! A.class_ "w-full max-w-3xl text-center mb-8" $ do
          when isOnboarding $ do
            H.h1 ! A.class_ "text-4xl font-bold text-primary" $ "Onboarding"
            H.h2 ! A.class_ "text-lg text-gray-700 mt-2" $ "Step 2 of 2"
          H.h2 ! A.class_ "text-xl font-semibold text-gray-900 mt-4" $ "Create Categories"
          H.p ! A.class_ "text-gray-600 mt-2" $ "Add categories for each account type."

        -- Config Section: Columns for Each Transaction Source
        H.div ! A.class_ "flex flex-col md:flex-row gap-6 w-full max-w-4xl" $ do
          forM_ (toList transactions) $ \(Entity sourceId source, categories) -> do
            H.div ! A.class_ "flex flex-col justify-around border border-primary rounded-md p-6 shadow-md w-full md:w-1/2" $ do
              H.h3 ! A.class_ "text-lg font-semibold text-primary mb-4" $ toHtml $ transactionSourceName source

              H.div ! A.class_ "flex flex-wrap gap-2" $ do
                forM_ categories $ \(Entity catId cat) -> do
                  H.form
                    ! A.method "post"
                    ! A.action (toValue $ "/remove-category/" <> show (fromSqlKey catId))
                    ! A.class_ "cursor-pointer"
                    $ do
                      H.button
                        ! A.type_ "submit"
                        ! A.name "removeCategory"
                        ! A.value (toValue $ categoryName cat)
                        ! A.class_ "w-full flex items-center gap-2 bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all focus:outline-none"
                        $ do
                          H.span $ toHtml $ categoryName cat
                          H.span ! A.class_ "text-red-500 font-bold" $ "×"

              let sourceName = transactionSourceName source
              when (sourceName `elem` ["Savings Account", "Checking Account"]) $
                renderEasyAdd sourceId categories ["Income", "Investments", "Credit Card Payments", "Transfers"]
              when (sourceName `elem` ["Credit Card", "Debit Card"]) $
                renderEasyAdd sourceId categories ["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food", "Credit Card Payments", "Entertainment"]

              -- Add Custom Category
              H.div ! A.class_ "mt-4" $ do
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
                  ! A.class_ "flex gap-2"
                  $ do
                    H.input
                      ! A.type_ "text"
                      ! A.name "newCategory"
                      ! A.placeholder "Category"
                      ! A.class_ "border border-gray-300 rounded-md p-2 flex-1"
                      ! A.required "required"
                    H.input
                      ! A.type_ "submit"
                      ! A.value "+"
                      ! A.class_ "primary-button"

        -- Navigation Buttons
        H.div ! A.class_ "flex flex-col sm:flex-row gap-4 mt-12 justify-center" $ do
          H.form
            ! A.method "get"
            ! A.action prevUrl
            $ do
              H.input
                ! A.type_ "submit"
                ! A.value "Previous"
                ! A.class_ "secondary-button"

          H.form
            ! A.method method
            ! A.action nextUrl
            $ do
              H.input
                ! A.type_ "submit"
                ! A.value "Finish"
                ! A.class_ "primary-button"

renderEasyAdd :: Key TransactionSource -> [Entity Category] -> [Text] -> Html
renderEasyAdd sourceId categories easyCategories = do
  H.div ! A.class_ "mt-4 flex flex-wrap gap-2" $ do
    forM_ easyCategories $ \easyCategory -> do
      let alreadyAdded = any (\(Entity _ cat) -> categoryName cat == easyCategory) categories
      unless alreadyAdded $ do
        H.form
          ! A.method "post"
          ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
          ! A.class_ "cursor-pointer"
          $ do
            H.button
              ! A.type_ "submit"
              ! A.name "newCategory"
              ! A.value (toValue easyCategory)
              ! A.class_ "w-full flex items-center gap-2 bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all focus:outline-none"
              $ do
                H.span $ toHtml easyCategory
                H.span ! A.class_ "text-green-600 font-bold" $ "+"
