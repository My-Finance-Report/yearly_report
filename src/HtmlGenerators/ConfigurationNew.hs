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
  Key SankeyConfig ->
  FullSankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  [Entity UploadConfiguration] ->
  [Entity TransactionSource] ->
  Html
renderConfigurationPageNew configId sankeyConfig transactions uploaderConfigs transactionSources = do
  H.body $ do
    H.div ! A.class_ "container mx-auto p-6" $ do
      H.h1 ! A.class_ "text-2xl font-bold text-gray-900 mb-4" $ "Flow Chart Configuration"
      renderEditSankeyConfigPage configId sankeyConfig transactions

renderEditSankeyConfigPage ::
  Key SankeyConfig ->
  FullSankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEditSankeyConfigPage configId config sourceCategories =
  H.div ! A.class_ "flex flex-col sm:flex-row gap-2 justify-center" $ do
    -- Inputs Section
    H.fieldset ! A.class_ "flex flex-col border border-gray-300 rounded-md p-4 max-w-[220px]" $ do
      H.legend ! A.class_ "font-semibold text-gray-700" $ "Inputs"

      H.p "These are the ways you get paid -- for example you both consult and have a day job "
      forM_ (inputs config) $ \(source, category) ->
        renderInputForm configId sourceCategories (Just (source, category))

      renderNewInputForm configId sourceCategories Nothing

    -- Linkages Section
    H.fieldset ! A.class_ "flex flex-col border border-gray-300 rounded-md p-4 max-w-[220px]" $ do
      H.legend ! A.class_ "font-semibold text-gray-700" $ "Linkages"
      H.p "Describes when money flows from one account to another, such as paying a credit card bill from a bank account"
      mapM_ (renderLinkageForm configId sourceCategories . Just) (linkages config)

      renderNewLinkageForm configId sourceCategories Nothing -- Add new linkage form

-- Individual Input Form
renderInputForm ::
  Key SankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Maybe (Entity TransactionSource, Entity Category) ->
  Html
renderInputForm configId sourceCategories maybeSelected = do
  H.form ! A.method "post" ! A.action "/remove-sankey-input" ! A.class_ "flex items-center gap-2 m-2" $ do
    -- Hidden Input for Config ID
    H.input
      ! A.type_ "hidden"
      ! A.name "sankeyConfigId"
      ! A.value (toValue $ fromSqlKey configId)

    -- Display the Source - Category as Read-Only
    case maybeSelected of
      Just (selectedSource, selectedCategory) -> do
        H.div ! A.class_ "bg-gray-100 text-gray-800 px-3 py-2 rounded-md flex-1" $ do
          H.input
            ! A.type_ "hidden"
            ! A.name "inputSourceId"
            ! A.value (toValue $ fromSqlKey (entityKey selectedSource))
          H.input
            ! A.type_ "hidden"
            ! A.name "inputCategoryId"
            ! A.value (toValue $ fromSqlKey (entityKey selectedCategory))
          H.toHtml $ transactionSourceName (entityVal selectedSource) <> " - " <> categoryName (entityVal selectedCategory)
      Nothing -> return ()

    -- Remove Button
    H.input
      ! A.type_ "submit"
      ! A.value "Remove"
      ! A.class_ "secondary-danger-button"
      ! A.formaction "/remove-sankey-input"

renderNewInputForm ::
  Key SankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Maybe (Entity TransactionSource, Entity Category) ->
  Html
renderNewInputForm configId sourceCategories maybeSelected = do
  H.form ! A.method "post" ! A.action "/add-sankey-input" ! A.class_ "flex items-center gap-2 m-2" $ do
    H.input
      ! A.type_ "hidden"
      ! A.name "sankeyConfigId"
      ! A.value (toValue $ fromSqlKey configId)

    H.select ! A.name "inputSourceCategory" ! A.class_ "border border-gray-300 rounded-md p-2 flex-1" $ do
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
    H.input ! A.type_ "submit" ! A.value "Add" ! A.class_ "secondary-button"

-- Individual Linkage Form (Read-Only Display)
renderLinkageForm ::
  Key SankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Maybe (Entity TransactionSource, Entity Category, Entity TransactionSource) ->
  Html
renderLinkageForm configId sourceCategories maybeSelected = do
  H.form ! A.method "post" ! A.action "/remove-sankey-linkage" ! A.class_ "flex items-center gap-2 m-2" $ do
    -- Hidden input for Sankey Config ID
    H.input
      ! A.type_ "hidden"
      ! A.name "sankeyConfigId"
      ! A.value (toValue $ fromSqlKey configId)

    -- Display the Source - Category - Target as Read-Only
    case maybeSelected of
      Just (selectedSource, selectedCategory, selectedTarget) -> do
        H.div ! A.class_ "bg-gray-100 text-gray-800 px-3 py-2 rounded-md" $ do
          toHtml $ transactionSourceName (entityVal selectedSource) <> " - " <> categoryName (entityVal selectedCategory) <> " → " <> transactionSourceName (entityVal selectedTarget)
      Nothing -> return ()

    case maybeSelected of
      Just (selectedSource, selectedCategory, selectedTarget) -> do
        H.div ! A.class_ "bg-gray-100 text-gray-800 px-3 py-2 rounded-md flex-1" $ do
          H.input
            ! A.type_ "hidden"
            ! A.name "inputSourceId"
            ! A.value (toValue $ fromSqlKey (entityKey selectedSource))
          H.input
            ! A.type_ "hidden"
            ! A.name "inputCategoryId"
            ! A.value (toValue $ fromSqlKey (entityKey selectedCategory))
          H.input
            ! A.type_ "hidden"
            ! A.name "targetSourceId"
            ! A.value (toValue $ fromSqlKey (entityKey selectedTarget))
      Nothing -> return ()

    -- Remove Button
    H.input
      ! A.type_ "submit"
      ! A.value "Remove"
      ! A.class_ "secondary-danger-button"
      ! A.formaction "/remove-sankey-linkage"

renderNewLinkageForm ::
  Key SankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Maybe (Entity TransactionSource, Entity Category, Entity TransactionSource) ->
  Html
renderNewLinkageForm configId sourceCategories maybeSelected = do
  H.form ! A.method "post" ! A.action "/add-sankey-linkage" ! A.class_ "flex items-center gap-2 m-2" $ do
    H.input
      ! A.type_ "hidden"
      ! A.name "sankeyConfigId"
      ! A.value (toValue $ fromSqlKey configId)

    H.select ! A.name "linkageSourceCategory" ! A.class_ "border border-gray-300 rounded-md p-2 flex-1" $ do
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

    H.select ! A.name "linkageTargetId" ! A.class_ "border border-gray-300 rounded-md p-2 flex-1" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        let isSelected = case maybeSelected of
              Just (_, _, selectedTarget) -> entityKey selectedTarget == sourceId
              Nothing -> False
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          !? (isSelected, A.selected "selected")
          $ toHtml
          $ transactionSourceName source
    H.input ! A.type_ "submit" ! A.value "Add" ! A.class_ "secondary-button"
