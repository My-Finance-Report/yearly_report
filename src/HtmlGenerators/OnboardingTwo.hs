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
          H.p ! A.class_ "text-gray-600 mt-2" $ "Add, edit, or remove categories for each account type."

        -- Full Column Layout for Transaction Sources
        H.div ! A.class_ "flex flex-col gap-6 w-full max-w-3xl" $ do
          forM_ (toList transactions) $ \(Entity sourceId source, categories) -> do
            H.div ! A.class_ "border border-primary rounded-md p-6 shadow-md w-full" $ do
              H.h3 ! A.class_ "text-lg font-semibold text-primary mb-4 text-center" $ toHtml $ transactionSourceName source

              -- Editable Categories List
              H.div ! A.class_ "flex flex-wrap gap-2 justify-center" $ do
                forM_ categories $ \(Entity catId cat) -> do
                  H.div ! A.class_ "flex items-center bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all w-full md:w-auto" $ do
                    -- Edit Form
                    H.form
                      ! A.method "post"
                      ! A.action (toValue $ "/edit-category/" <> show (fromSqlKey catId))
                      ! A.class_ "flex flex-1 items-center gap-2"
                      $ do
                        -- Editable Input Field for Category Name
                        H.input
                          ! A.type_ "text"
                          ! A.name "updatedCategoryName"
                          ! A.value (toValue $ categoryName cat)
                          ! A.class_ "border border-gray-300 rounded-md p-2 flex-1"

                        -- Save Button
                        H.input
                          ! A.type_ "submit"
                          ! A.value "Update"
                          ! A.class_ "secondary-button text-sm"

                    -- Remove Form
                    H.form
                      ! A.method "post"
                      ! A.action (toValue $ "/remove-category/" <> show (fromSqlKey catId))
                      ! A.class_ "ml-2"
                      $ do
                        H.input
                          ! A.type_ "hidden"
                          ! A.name "removeCategory"
                          ! A.value (toValue $ categoryName cat)
                        H.input
                          ! A.type_ "submit"
                          ! A.value "Remove"
                          ! A.class_ "secondary-danger-button"

              -- Add New Category (Always at the Bottom)
              H.div ! A.class_ "flex items-center bg-gray-100 text-gray-800 px-3 py-2 rounded-md hover:bg-gray-200 transition-all mt-4" $ do
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
                  ! A.class_ "flex gap-2 w-full"
                  $ do
                    H.input
                      ! A.type_ "text"
                      ! A.name "newCategory"
                      ! A.placeholder "Category"
                      ! A.class_ "border border-gray-300 rounded-md p-2 flex-1"
                      ! A.required "required"
                    H.input
                      ! A.type_ "submit"
                      ! A.value "Add"
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


