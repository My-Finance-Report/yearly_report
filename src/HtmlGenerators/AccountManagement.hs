{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.AccountManagement (renderAccountManagement, renderCustomAccountForm) where

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
        -- Iterate over grouped accounts
        forM_ (toList groupedTransactions) $ \(kind, sources) -> do
          H.div ! A.class_ "w-full max-w-3xl border-l-4 border-green-500 p-3 mb-4" $ do
            -- Group Header
            H.h2 ! A.class_ "text-xl font-semibold text-primary mb-2 text-center border-b pb-1" $ toHtml $ show kind

            -- Add New Account Form
            renderCustomAccountForm kind

            -- List Accounts Under Each Group
            H.div ! A.class_ "flex flex-col gap-2" $ do
              forM_ sources $ \sourceEntity@(Entity sourceId source) -> do
                let categories = findWithDefault [] sourceEntity transactions

                H.div ! A.class_ "border border-primary rounded-md p-3 shadow-md w-full bg-white" $ do
                  -- Edit Transaction Source Form
                  H.form
                    ! A.method "post"
                    ! A.action (toValue $ "/edit-transaction-source/" <> show (fromSqlKey sourceId))
                    ! A.class_ "flex items-center gap-2"
                    $ do
                      -- Editable Input Field for Name
                      H.input
                        ! A.type_ "text"
                        ! A.name "updatedSourceName"
                        ! A.value (toValue $ transactionSourceName source)
                        ! A.class_ "border border-gray-300 rounded-md p-1 flex-1 text-sm"

                      H.input
                        ! A.type_ "hidden"
                        ! A.name "sourceKind"
                        ! A.value (toValue $ show kind)

                      -- Save Button
                      H.input
                        ! A.type_ "submit"
                        ! A.value "Update"
                        ! A.class_ "secondary-button text-sm"

                  -- Remove Form
                  H.form
                    ! A.method "post"
                    ! A.action "/remove-transaction-source"
                    ! A.class_ "mt-1 flex items-center"
                    $ do
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
                        ! A.class_ "secondary-danger-button text-sm"

                  -- Editable Categories List
                  H.div ! A.class_ "flex flex-wrap gap-1 mt-1" $ do
                    forM_ categories $ \(Entity catId cat) -> do
                      H.div ! A.class_ "flex items-center bg-gray-100 text-gray-800 px-2 py-1 rounded-md hover:bg-gray-200 transition-all text-sm" $ do
                        -- Edit Form
                        H.form
                          ! A.method "post"
                          ! A.action (toValue $ "/edit-category/" <> show (fromSqlKey catId))
                          ! A.class_ "flex flex-1 items-center gap-1"
                          $ do
                            -- Editable Input Field for Category Name
                            H.input
                              ! A.type_ "text"
                              ! A.name "updatedCategoryName"
                              ! A.value (toValue $ categoryName cat)
                              ! A.class_ "border border-gray-300 rounded-md p-1 flex-1 text-sm"

                            -- Save Button
                            H.input
                              ! A.type_ "submit"
                              ! A.value "Update"
                              ! A.class_ "secondary-button text-xs"

                        -- Remove Form
                        H.form
                          ! A.method "post"
                          ! A.action (toValue $ "/remove-category/" <> show (fromSqlKey catId))
                          ! A.class_ "ml-1"
                          $ do
                            H.input
                              ! A.type_ "hidden"
                              ! A.name "removeCategory"
                              ! A.value (toValue $ categoryName cat)
                            H.input
                              ! A.type_ "submit"
                              ! A.value "Remove"
                              ! A.class_ "secondary-danger-button text-xs"

                  -- Add New Category
                  H.div ! A.class_ "mt-2" $ do
                    H.form
                      ! A.method "post"
                      ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
                      ! A.class_ "flex gap-1 w-full"
                      $ do
                        H.input
                          ! A.type_ "text"
                          ! A.name "newCategory"
                          ! A.placeholder "Enter new category"
                          ! A.class_ "border border-gray-300 rounded-md p-1 flex-1 text-sm"
                          ! A.required "required"
                        H.input
                          ! A.type_ "submit"
                          ! A.value "Add New Category"
                          ! A.class_ "primary-button text-s"

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
      ! A.placeholder "Enter account name"
      ! A.class_ "min-w-96 border border-gray-300 rounded-md p-2 flex-1"
    H.input
      ! A.type_ "hidden"
      ! A.name "newKind"
      ! A.value (toValue $ show kind)
    H.input
      ! A.type_ "submit"
      ! A.value "Add New Source"
      ! A.class_ "primary-button text-s"
