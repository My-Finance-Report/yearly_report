{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.LandingPage (renderLandingPage) where

import Data.Text (Text)
import Text.Blaze.Html5 as H
  ( Html,
    ToValue (toValue),
    body,
    br,
    div,
    form,
    h1,
    h2,
    h3,
    input,
    button,
    label,
    li,
    option,
    p,
    pre,
    script,
    select,
    span,
    toHtml,
    ul,
    (!),
  )
import Text.Blaze.Html5.Attributes as A
  ( action,
    class_,
    for,
    id,
    method,
    name,
    oninput,
    onclick,
    placeholder,
    src,
    type_,
    value,
  )

renderLandingPage :: Html
renderLandingPage = 
  H.body $ do
    H.div ! A.class_ "container landing-page" $ do
      -- Banner
      H.div ! A.class_ "banner" $ do
        H.h2 "Simple financial reporting for just $2/month or $20/year." 
        H.p "Because if it's free, someone is probably selling your data."

      -- Features Section with Cards
      H.div ! A.class_ "features-section" $ do
        H.div ! A.class_ "cards-container" $ do
          featureCard "Parse PDF bank statements and credit card statements" "Easily upload and process your financial statements for a clear breakdown."
          featureCard "Plaid integration for real-time bank data" "Sync your bank account data seamlessly in real time with Plaid."
          featureCard "Categorize expenses effortlessly" "Smart categorization makes managing expenses a breeze."
          featureCard "Generate detailed reports" "Drill down into your expenses with per-month summaries and per-transaction insights."

      -- Call-to-action Section
      H.div ! A.class_ "cta-section" $ do
        H.h3 "Try before you buy?"
        H.p "Explore my finances (yes these are my actual finances)."
        H.button
          ! A.class_ "btn explore-btn"
          ! A.onclick "window.location.href='/demo-account'"
          $ "Explore"


-- Helper function to generate a feature card
featureCard :: Text -> Text -> Html
featureCard title description =
  H.div ! A.class_ "card" $ do
    H.h3 $ toHtml title
    H.p $ toHtml description
