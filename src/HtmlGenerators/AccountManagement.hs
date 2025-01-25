{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AccountManagement (renderAccountManagement) where

import Control.Monad (forM_)
import Data.Map (Map, findWithDefault, fromListWith, keys, toList)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (fromSqlKey)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderAccountManagement :: Entity User -> Map (Entity TransactionSource) [Entity Category] -> Bool -> Html
renderAccountManagement user transactions isOnboarding =
  let groupedTransactions = fromListWith (++) [(transactionSourceSourceKind (entityVal ts), [ts]) | ts <- keys transactions]
   in H.div ! A.class_ "bg-gray-50 text-gray-900 min-h-screen flex flex-col items-center p-4" $ do
        H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty
        H.div ! A.class_ "w-full max-w-3xl flex gap-4 ml-3" $ do
          forM_ (zip [0 ..] (toList groupedTransactions)) $ \(idx, (kind, _)) ->
            H.button
              ! A.class_ ("tab px-4 py-2 text-lg border border-gray-300 bg-gray-100 hover:bg-white hover:text-primary cursor-pointer rounded-t-lg" <> if idx == 0 then " active" else "")
              ! A.onclick (toValue $ "showTabWithSubtabs(" <> show idx <> ")")
              $ toHtml
              $ show kind <> "s"

        forM_ (zip [0 ..] (toList groupedTransactions)) $ \(idx, (kind, sources)) ->
          H.div
            ! A.class_ ("tab-content w-full p-5 rounded-lg border border-primary max-w-3xl" <> if idx == 0 then " block" else " hidden")
            ! A.id (toValue $ "tab-content-" <> show idx)
            $ do
              renderNewAccountForm kind

              H.div ! A.class_ "flex flex-col gap-2 items-center" $ do
                forM_ sources $ \sourceEntity@(Entity sourceId source) -> do
                  let categories = findWithDefault [] sourceEntity transactions
                  H.div ! A.class_ "border border-primary rounded-md p-3 shadow-md bg-white w-full" $ do
                    renderTransactionSourceForm sourceEntity

                    H.div ! A.class_ "flex flex-wrap gap-1 mt-1" $ do
                      forM_ categories $ \category ->
                        renderCategoryForm category

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

renderTransactionSourceForm :: Entity TransactionSource -> Html
renderTransactionSourceForm source = do
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
          ! A.class_ "min-w-[350px] border border-gray-300 rounded-md p-1 flex-1 text-sm"

        H.input
          ! A.type_ "hidden"
          ! A.name "sourceKind"
          ! A.value (toValue $ show (transactionSourceSourceKind $ entityVal source))

        -- Save Button
        H.input
          ! A.type_ "submit"
          ! A.value "Update"
          ! A.class_ "secondary-button text-sm"

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
          ! A.class_ "border border-gray-300 rounded-md p-1 flex-1 text-sm"

        -- Save Button
        H.input
          ! A.type_ "submit"
          ! A.value "Update"
          ! A.class_ "secondary-button text-xs"

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