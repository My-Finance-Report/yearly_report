{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingTwo (renderOnboardingTwo) where

import Control.Monad (forM_)
import Data.Map (Map, toList)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderOnboardingTwo :: Entity User -> Map (Entity TransactionSource) [Entity Category] -> Html
renderOnboardingTwo user transactions =
  H.body $ do
    -- Include the CSS for onboarding
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/css/onboarding.css"

    -- Page Header
    H.div ! A.class_ "page-header" $ do
      H.h1 "Onboarding"
      H.h2 "Step 2 of 3"
      H.h2 "Create Categories"
      H.p "Organize your transactions by adding categories to each account type."

    -- Config Section: Columns for Each Transaction Source
    H.div ! A.class_ "config-columns" $ do
      forM_ (toList transactions) $ \(Entity sourceId source, categories) -> do
        H.div ! A.class_ "config-column" $ do
          H.h3 $ toHtml $ transactionSourceName source

          -- Existing Categories
          H.div ! A.class_ "category-list" $ do
            forM_ categories $ \(Entity catId cat) -> do
              H.div ! A.class_ "category-card" $ do
                H.form
                  ! A.class_ "card-content"
                  ! A.method "post"
                  ! A.action (toValue $ "/remove-category/" <> show (fromSqlKey catId))
                  $ do
                    H.span $ toHtml $ categoryName cat
                    H.input
                      ! A.type_ "submit"
                      ! A.value "-"
                      ! A.class_ "btn-delete"

          let sourceName = transactionSourceName source
          if sourceName == "Savings Account" || sourceName == "Checking Account"
            then renderEasyAdd sourceId categories ["Income", "Investments", "Credit Card Payments", "Transfers"]
            else
              if sourceName == "Credit Card" || sourceName == "Debit Card"
                then renderEasyAdd sourceId categories ["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food"]
                else return ()

          H.div ! A.class_ "category-card" $ do
            H.form
              ! A.class_ "card-content"
              ! A.method "post"
              ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
              $ do
                H.input
                  ! A.type_ "text"
                  ! A.name "newCategory"
                  ! A.placeholder "Category"
                  ! A.class_ "free-form-input"
                  ! A.required "required"
                H.input
                  ! A.type_ "submit"
                  ! A.value "+"
                  ! A.class_ "btn-submit"

    H.div ! A.class_ "button-container" $ do
      H.form
        ! A.method "get"
        ! A.action "/onboarding/step-1"
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Previous"
            ! A.class_ "btn-next"

      H.form
        ! A.method "post"
        ! A.action "/onboarding/step-2"
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Next"
            ! A.class_ "btn-next"

renderEasyAdd :: Key TransactionSource -> [Entity Category] -> [Text] -> Html
renderEasyAdd sourceId categories easyCategories = do
  H.div ! A.class_ "easy-add" $ do
    forM_ easyCategories $ \easyCategory -> do
      let alreadyAdded = any (\(Entity _ cat) -> categoryName cat == easyCategory) categories
      if alreadyAdded
        then return ()
        else H.div ! A.class_ "category-card" $ do
          H.form
            ! A.class_ "card-content"
            ! A.method "post"
            ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
            $ do
              H.span $ toHtml easyCategory
              H.input
                ! A.type_ "hidden"
                ! A.name "newCategory"
                ! A.value (toValue easyCategory)
              H.input ! A.type_ "submit" ! A.value "+" ! A.class_ "btn-submit"
