{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.LandingPage (renderLandingPage) where

import Data.Text (Text)
import Text.Blaze.Html5 as H
  ( Html,
    ToValue (toValue),
    body,
    br,
    button,
    div,
    form,
    h1,
    h2,
    h3,
    input,
    label,
    li,
    link,
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
    href,
    id,
    method,
    name,
    onclick,
    oninput,
    placeholder,
    rel,
    src,
    type_,
    value,
  )

renderLandingPage :: Html
renderLandingPage =
  H.body $ do
    H.link
      ! A.rel "stylesheet"
      ! A.type_ "text/css"
      ! A.href "/css/landing.css"
    H.div ! A.class_ "container landing-page" $ do
      -- Banner
      H.div ! A.class_ "banner" $ do
        H.h1 "My Financé"
        H.h2 "Because you should love your finances"
        H.h2 ! A.class_ "subtag" $ "Simple personal finance for just $2/month or $20/year."

      H.div ! A.class_ "features-section" $ do
        H.div ! A.class_ "flex flex-col sm:flex-row gap-2" $ do
          featureCard "Upload Transactions" "Automatically pull transactions from your bank statements"
          featureCard "Categorize Expenses" "Automatically categorize all your expenses"
          featureCard "See The Money Flow" "Understand where your money is actually going!"

        H.div ! A.class_ "cta-section" $ do
          H.h3 "Explore the sandbox!"
          H.div ! A.class_ "button-group" $ do
            H.button
              ! A.class_ "explore-btn"
              ! A.onclick "window.location.href='/demo-account'"
              $ "Explore"
            H.button
              ! A.class_ "login-btn"
              ! A.onclick "window.location.href='/login'"
              $ "Sign up"

-- Helper function to generate a feature card
featureCard :: Text -> Text -> Html
featureCard title description =
  H.div ! A.class_ "card" $ do
    H.h3 $ toHtml title
    H.p $ toHtml description
