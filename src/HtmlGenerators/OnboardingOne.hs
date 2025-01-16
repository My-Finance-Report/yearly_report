{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.OnboardingOne (renderOnboardingOne, newSourceComponent) where

import Control.Monad (forM_)
import Data.Text (Text)
import Database.Models
import Database.Persist (Entity (..))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderPrefilledCard :: Text -> [Entity TransactionSource] -> Html
renderPrefilledCard name transactionSources =
  if name `elem` Prelude.map (transactionSourceName . entityVal) transactionSources
    then return ()
    else H.div ! A.class_ "card" $ do
      H.h3 $ toHtml name
      H.form
        ! A.method "post"
        ! A.action "/add-transaction-source"
        $ do
          H.div ! A.class_ "form-group" $ do
            H.input
              ! A.type_ "hidden"
              ! A.name "newSource"
              ! A.value (toValue name)
          H.input
            ! A.type_ "submit"
            ! A.value "Add"
            ! A.class_ "btn-submit"

newSourceComponent :: [Entity TransactionSource] -> Bool -> Html
newSourceComponent transactionSources includeDefaults=
    H.div ! A.class_ "card-container" $ do
      forM_ transactionSources $ \source -> do
        let txnName = transactionSourceName $ entityVal source

        H.div ! A.class_ "card" $ do
          H.h3 $ toHtml txnName
          H.form
            ! A.method "post"
            ! A.action "/remove-transaction-source"
            $ do
              H.div ! A.class_ "form-group" $ do
                H.input
                  ! A.type_ "hidden"
                  ! A.name "newSource"
                  ! A.value (toValue txnName)
              H.input
                ! A.type_ "submit"
                ! A.value "Remove"
                ! A.class_ "btn-delete"

      case includeDefaults of
        True -> do
                renderPrefilledCard "Credit Card" transactionSources
                renderPrefilledCard "Debit Card" transactionSources
                renderPrefilledCard "Savings Account" transactionSources
                renderPrefilledCard "Checking Account" transactionSources
        False -> return ()

      H.div ! A.class_ "card" $ do
        H.form
          ! A.method "post"
          ! A.action "/add-transaction-source"
          $ do
            H.div ! A.class_ "form-group" $ do
              H.input
                ! A.type_ "Text"
                ! A.placeholder "account name"
                ! A.name "newSource"
                ! A.required "required"
            H.input
              ! A.type_ "submit"
              ! A.value "Add"
              ! A.class_ "btn-submit"


renderOnboardingOne :: Entity User -> [Entity TransactionSource] -> Bool -> Html
renderOnboardingOne user transactionSources isOnboarding=
  let nextUrl = if isOnboarding 
                  then "/onboarding/step-2"
                  else "/add-account/step-2"
      method = if isOnboarding
                  then "post" 
                  else "get"



  in H.body $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/css/onboarding.css"

    H.div ! A.class_ "page-header" $ do
      case isOnboarding of
        True -> do
              H.h1 "Onboarding"
              H.h2 "Step 1 of 3"
        False -> return ()
      H.h2 "Add Accounts"
      H.p "Think Saving Account, Checking Account, Credit Card, etc."


      newSourceComponent transactionSources isOnboarding


    H.div ! A.class_ "next-button-container" $ do
      H.form
        ! A.method method
        ! A.action nextUrl
        $ do
          H.input
            ! A.type_ "submit"
            ! A.value "Next"
            ! A.class_ "btn-next"

