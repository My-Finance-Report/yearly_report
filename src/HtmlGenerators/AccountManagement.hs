{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AccountManagement (renderAccountManagement) where

import Control.Monad (forM_)
import Data.List (nub)
import Data.Map (Map, findWithDefault, fromListWith, keys, lookup, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderAccountManagement ::
  Entity User ->
  [SourceKind] ->
  Map (Entity TransactionSource) [Entity Category] ->
  Bool ->
  Html
renderAccountManagement user kinds transactions isOnboarding =
  let -- Group the existing sources by their SourceKind
      groupedTransactions :: Map SourceKind [Entity TransactionSource]
      groupedTransactions =
        fromListWith
          (++)
          [ (transactionSourceSourceKind (entityVal ts), [ts])
            | ts <- keys transactions
          ]

      allKinds :: [SourceKind]
      allKinds = nub $ kinds ++ keys groupedTransactions
   in H.div ! A.class_ "bg-gray-50 text-gray-900 min-h-screen flex flex-col items-center p-4" $ do
        -- (1) Load your new tabs.js file with showTabWithSubtabs()
        H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty

        -- (2) Button group: one button per SourceKind
        H.div ! A.class_ "flex flex-row items-center justify-center mt-4" $ do
          H.div ! A.class_ "flex flex-row gap-2 text-primary border-primary rounded-md border-[1px] p-4 bg-white shadow-sm" $ do
            forM_ (Prelude.zip [0 ..] allKinds) $ \(idx, kind) -> do
              let sources = fromMaybe [] (Data.Map.lookup kind groupedTransactions)
                  count = Prelude.length sources

              H.button
                ! A.type_ "button"
                ! H.dataAttribute "tab-index" (toValue $ Prelude.show idx)
                ! A.class_
                  "tab-button secondary-button flex items-center gap-2 px-4 py-2 border border-gray-300 \
                  \bg-gray-100 hover:bg-white hover:text-primary cursor-pointer rounded-md"
                ! A.onclick (toValue $ "showTabWithSubtabs(" <> Prelude.show idx <> ")")
                $ do
                  toHtml (show kind <> "s")
                  H.span
                    ! A.class_ "text-xs bg-primary text-white w-5 h-5 flex items-center justify-center rounded-full"
                    $ toHtml (Prelude.show count)

        -- (3) Tab Content container
        H.div ! A.class_ "border border-primary p-2 rounded-md mt-4 w-full max-w-3xl" $ do
          -- For each kind, generate a <div> with id="tab-content-idx"
          -- The first tab is shown by default (style="display:block")
          forM_ (Prelude.zip [0 ..] allKinds) $ \(idx, kind) -> do
            H.div
              ! A.class_ "tab-content w-full p-5 rounded-lg border border-primary max-w-3xl"
              ! A.id (toValue $ "tab-content-" <> Prelude.show idx)
              ! A.style (if idx == 0 then "display: block;" else "display: none;")
              $ do
                -- (a) A form for creating a new account of this kind
                renderNewAccountForm kind

                -- (b) Display each existing TransactionSource + categories
                let sources = fromMaybe [] (Data.Map.lookup kind groupedTransactions)
                H.div ! A.class_ "flex flex-col gap-2 items-center" $ do
                  forM_ sources $ \sourceEntity@(Entity sourceId source) -> do
                    let categories = findWithDefault [] sourceEntity transactions
                    H.div ! A.class_ "border border-primary rounded-md p-3 shadow-md bg-white w-full" $ do
                      -- Show the existing source form
                      renderTransactionSourceForm sourceEntity kinds

                      -- List all categories
                      H.div ! A.class_ "flex flex-wrap gap-1 mt-1" $ do
                        forM_ categories $ \catEntity ->
                          renderCategoryForm catEntity

                      -- Button or form for new category
                      renderNewCategoryForm sourceId

renderNewAccountForm :: SourceKind -> Html
renderNewAccountForm kind = do
  H.form
    ! A.method "post"
    ! A.action "/add-transaction-source"
    ! A.class_ "flex items-center gap-2 border border-primary p-3 rounded-md cursor-pointer my-2"
  $ do
    H.input
      ! A.type_ "text"
      ! A.name "newSource"
      ! A.placeholder ("New " <> toValue (show kind))
      ! A.class_ "min-w-96 border border-gray-300 rounded-md p-2 flex-1"
    H.input
      ! A.type_ "hidden"
      ! A.name "newKind"
      ! A.value (toValue $ show kind)
    H.input
      ! A.type_ "submit"
      ! A.value ("Add New " <> toValue (show kind))
      ! A.class_ "primary-button text-s"

renderTransactionSourceForm :: Entity TransactionSource -> [SourceKind] -> Html
renderTransactionSourceForm source kinds = do
  H.div ! A.class_ "flex gap-2 my-3 items-center" $ do
    H.form
      ! A.method "post"
      ! A.action (toValue $ "/edit-transaction-source/" <> show (fromSqlKey $ entityKey source))
      ! A.class_ "flex items-center gap-2"
      $ do
        -- Editable Input Field for Name
        H.input
          ! A.type_ "text"
          ! A.name "updatedSourceName"
          ! A.value (toValue $ transactionSourceName $ entityVal source)
          ! A.class_ "edit-input min-w-[250px] border border-gray-300 rounded-md p-1 flex-1 text-sm"
          ! A.required "required"
          ! A.oninput "toggleUpdateButton(this)"

        -- Dropdown for Selecting Source Kind
        H.select
          ! A.name "updatedSourceKind"
          ! A.class_ "edit-input border border-gray-300 rounded-md p-1 text-sm bg-white"
          ! A.oninput "toggleUpdateButton(this)"
          $ forM_ kinds
          $ \kind ->
            H.option
              ! A.value (toValue $ show kind)
              !? (kind == transactionSourceSourceKind (entityVal source), A.selected "selected")
              $ toHtml (show kind)

        -- Save Button
        H.input
          ! A.type_ "submit"
          ! A.value "Update"
          ! A.class_ "update-button secondary-button text-sm"
          ! A.disabled "true"

    -- Remove Form
    H.form
      ! A.method "post"
      ! A.action "/remove-transaction-source"
      ! A.class_ "flex items-center"
      $ do
        H.input
          ! A.type_ "hidden"
          ! A.name "sourceName"
          ! A.value (toValue $ transactionSourceName $ entityVal source)
        H.input
          ! A.type_ "hidden"
          ! A.name "sourceKind"
          ! A.value (toValue $ show $ transactionSourceSourceKind $ entityVal source)

        -- Remove Button
        H.input
          ! A.type_ "submit"
          ! A.value "Remove"
          ! A.class_ "secondary-danger-button text-sm"

renderCategoryForm :: Entity Category -> Html
renderCategoryForm category = do
  H.div ! A.class_ "flex items-center bg-gray-100 text-gray-800 px-2 py-1 rounded-md hover:bg-gray-200 transition-all text-sm" $ do
    -- Edit Form
    H.form
      ! A.method "post"
      ! A.action (toValue $ "/edit-category/" <> show (fromSqlKey $ entityKey category))
      ! A.class_ "flex flex-1 items-center gap-1"
      $ do
        -- Editable Input Field for Category Name
        H.input
          ! A.type_ "text"
          ! A.name "updatedCategoryName"
          ! A.value (toValue $ categoryName $ entityVal category)
          ! A.class_ "edit-input border border-gray-300 rounded-md p-1 flex-1 text-sm"
          ! A.oninput "toggleUpdateButton(this)"

        -- Save Button
        H.input
          ! A.type_ "submit"
          ! A.value "Update"
          ! A.class_ "update-button secondary-button text-xs"
          ! A.disabled "true"

    -- Remove Form
    H.form
      ! A.method "post"
      ! A.action (toValue $ "/remove-category/" <> show (fromSqlKey $ entityKey category))
      ! A.class_ "ml-1"
      $ do
        H.input
          ! A.type_ "hidden"
          ! A.name "removeCategory"
          ! A.value (toValue $ categoryName $ entityVal category)
        H.input
          ! A.type_ "submit"
          ! A.value "Remove"
          ! A.class_ "secondary-danger-button text-xs"

renderNewCategoryForm :: Key TransactionSource -> Html
renderNewCategoryForm sourceId = do
  H.div ! A.class_ "mt-2" $ do
    H.form
      ! A.method "post"
      ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
      ! A.class_ "flex gap-1 w-full"
      $ do
        H.input
          ! A.type_ "text"
          ! A.name "newCategory"
          ! A.placeholder "New Category"
          ! A.class_ "border border-gray-300 rounded-md p-1 flex-1 text-sm"
          ! A.required "required"
        H.input
          ! A.type_ "submit"
          ! A.value "Add New Category"
          ! A.class_ "primary-button text-s"