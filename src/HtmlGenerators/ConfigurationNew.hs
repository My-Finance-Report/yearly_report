{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.ConfigurationNew (renderConfigurationPageNew) where

import Control.Monad (forM_)
import Data.Map hiding ((!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey)
import HtmlGenerators.OnboardingOne
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

renderConfigurationPageNew ::
  Maybe FullSankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  [Entity UploadConfiguration] ->
  [Entity TransactionSource] ->
  Html
renderConfigurationPageNew sankeyConfig transactions uploaderConfigs transactionSources = do
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.h1 "Configuration Page"

      renderEditSankeyConfigPage sankeyConfig transactions

renderEditSankeyConfigPage ::
  Maybe FullSankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEditSankeyConfigPage maybeConfig sourceCategories =
  H.div $ do
    H.h1 "Edit or Create Sankey Configuration"

    H.form
      ! A.method "post"
      ! A.action "/update-sankey-config"
      $ do
        -- Inputs Section
        H.fieldset $ do
          H.legend "Inputs"
          case maybeConfig of
            Just FullSankeyConfig {inputs} ->
              forM_ inputs $ \(source, category) ->
                renderInputRow sourceCategories (Just (source, category))
            Nothing -> renderEmptyInputRow sourceCategories

          -- Add New Input Button
          H.button
            ! A.type_ "button"
            ! A.onclick "addInputRow()"
            $ "Add Input"

        H.br

        -- Linkages Section
        H.fieldset $ do
          H.legend "Linkages"
          case maybeConfig of
            Just FullSankeyConfig {linkages = (source, category, target)} ->
              renderLinkageRow sourceCategories (Just (source, category, target))
            Nothing -> renderEmptyLinkageRow sourceCategories

        H.br
        -- Submit Button
        H.input ! A.type_ "submit" ! A.value "Save Changes"

-- Simplified Dropdown for Input Pairs (TransactionSource - Category)
renderInputRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Maybe (Entity TransactionSource, Entity Category) ->
  Html
renderInputRow sourceCategories maybeSelected = do
  H.div $ do
    H.select ! A.name "inputSourceCategory[]" $ do
      forM_ (Map.toList sourceCategories) $ \(Entity sourceId source, categories) ->
        forM_ categories $ \(Entity categoryId category) -> do
          let optionValue = toValue (fromSqlKey sourceId) <> "-" <> toValue (fromSqlKey categoryId)
          let isSelected = case maybeSelected of
                Just (selectedSource, selectedCategory) ->
                  entityKey selectedSource == sourceId && entityKey selectedCategory == categoryId
                Nothing -> False
          H.option
            ! A.value optionValue
            !? (isSelected, A.selected "selected")
            $ toHtml
            $ transactionSourceName source <> " - " <> categoryName category

-- Render Empty Row for New Input
renderEmptyInputRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEmptyInputRow sourceCategories = do
  H.div $ do
    H.select ! A.name "inputSourceCategory[]" $ do
      forM_ (Map.toList sourceCategories) $ \(Entity sourceId source, categories) ->
        forM_ categories $ \(Entity categoryId category) -> do
          let optionValue = toValue (fromSqlKey sourceId) <> "-" <> toValue (fromSqlKey categoryId)
          H.option
            ! A.value optionValue
            $ toHtml
            $ transactionSourceName source <> " - " <> categoryName category

-- Linkage Dropdowns (Using Composite Source-Category Field)
renderLinkageRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Maybe (Entity TransactionSource, Entity Category, Entity TransactionSource) ->
  Html
renderLinkageRow sourceCategories maybeSelected = do
  H.div $ do
    H.select ! A.name "linkageSourceCategory" $ do
      forM_ (Map.toList sourceCategories) $ \(Entity sourceId source, categories) ->
        forM_ categories $ \(Entity categoryId category) -> do
          let optionValue = toValue (fromSqlKey sourceId) <> "-" <> toValue (fromSqlKey categoryId)
          let isSelected = case maybeSelected of
                Just (selectedSource, selectedCategory, _) ->
                  entityKey selectedSource == sourceId && entityKey selectedCategory == categoryId
                Nothing -> False
          H.option
            ! A.value optionValue
            !? (isSelected, A.selected "selected")
            $ toHtml
            $ transactionSourceName source <> " - " <> categoryName category

    H.select ! A.name "linkageTargetId" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        let isSelected = case maybeSelected of
              Just (_, _, selectedTarget) -> entityKey selectedTarget == sourceId
              Nothing -> False
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          !? (isSelected, A.selected "selected")
          $ toHtml
          $ transactionSourceName source

renderEmptyLinkageRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEmptyLinkageRow sourceCategories = do
  H.div $ do
    H.select ! A.name "linkageSourceCategory" $ do
      forM_ (Map.toList sourceCategories) $ \(Entity sourceId source, categories) ->
        forM_ categories $ \(Entity categoryId category) -> do
          let optionValue = toValue (fromSqlKey sourceId) <> "-" <> toValue (fromSqlKey categoryId)
          H.option
            ! A.value optionValue
            $ toHtml
            $ transactionSourceName source <> " - " <> categoryName category

    H.select ! A.name "linkageTargetId" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          $ toHtml
          $ transactionSourceName source
