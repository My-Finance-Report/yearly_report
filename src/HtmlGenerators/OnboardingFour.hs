{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingFour (renderOnboardingFour) where

import Control.Monad (forM_)
import Data.Map (Map, keys, toList)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderOnboardingFour :: Entity User -> Map (Entity TransactionSource) [Entity Category] -> Html
renderOnboardingFour user sourceCategoriesMap =
  H.body $ do
    H.script H.! A.type_ "text/javascript" H.! A.src "/onboarding.js" $ ""

    -- Page Header
    H.div ! A.class_ "page-header" $ do
      H.h1 "Onboarding"
      H.h2 "Step 4 of 4"
      H.h2 "Define Financial Flows"
      H.p "Organize your finances by selecting income sources and configuring connections."

    -- Top Row: All Source-Category Pairs
    H.div ! A.class_ "top-row" $ do
      H.h3 "All Categories"
      H.div ! A.class_ "category-pair-list" $ do
        forM_ (toList sourceCategoriesMap) $ \(Entity sourceId source, categories) -> do
          forM_ categories $ \(Entity catId cat) -> do
            H.div ! A.class_ "category-pair-card" $ do
              H.span $ toHtml $ transactionSourceName source <> " - " <> categoryName cat
              H.button
                ! A.class_ "btn-add"
                ! H.dataAttribute "source-id" (toValue $ fromSqlKey sourceId)
                ! H.dataAttribute "category-id" (toValue $ fromSqlKey catId)
                ! H.dataAttribute "category-name" (toValue $ categoryName cat)
                $ "+"

    -- Main Columns
    H.div ! A.class_ "main-columns" $ do
      -- Left Column: Income Sources
      H.div ! A.id "income-sources" ! A.class_ "column" $ do
        H.h3 "Income Sources"
        H.div ! A.class_ "income-list" $ do
          H.p "No income sources selected yet."

      -- Right Column: Configure Connections
      H.div ! A.id "config-dropdowns" ! A.class_ "column" $ do
        H.h3 "Configure Connections"

        -- First Dropdown: Select Source
        H.div ! A.class_ "dropdown-container" $ do
          H.label ! A.for "source-dropdown" $ "Select Source"
          H.select ! A.id "source-dropdown" $ do
            H.option ! A.value "" $ "Select Source"
            forM_ (keys sourceCategoriesMap) $ \(Entity sourceId source) -> do
              H.option
                ! A.value (toValue $ fromSqlKey sourceId)
                $ toHtml
                $ transactionSourceName source

        -- Second Dropdown: Select Category
        H.div ! A.class_ "dropdown-container" $ do
          H.label ! A.for "source-category-dropdown" $ "Select Category"
          H.select ! A.id "source-category-dropdown" $ do
            H.option ! A.value "" $ "Select Category"

        -- Third Dropdown: Select Destination Source
        H.div ! A.class_ "dropdown-container" $ do
          H.label ! A.for "destination-dropdown" $ "Select Destination Source"
          H.select ! A.id "destination-dropdown" $ do
            H.option ! A.value "" $ "Select Destination"

        -- Connect Button
        H.button
          ! A.id "connect-button"
          ! A.class_ "btn-connect"
          ! A.disabled "disabled"
          $ "Connect"
