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
    (!), link,
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
    value, rel, href,
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
        H.h3 "Simple financial reporting for just $2/month or $20/year." 

      H.div ! A.class_ "features-section" $ do
        H.div ! A.class_ "cards-container" $ do
          featureCard "Process Statements" "Upload a statement and we do the rest"
          featureCard "Categorize expenses automatically" "Create custom categorizes and group transactions"
          featureCard "Generate detailed reports" "Understand where your money is actually going!"

      -- Call-to-action Section
      H.div ! A.class_ "cta-section" $ do
        H.h3 "Try before you buy?"
        H.p "Explore my finances"
        H.div ! A.class_ "button-group" $ do
          H.button
            ! A.class_ "explore-btn"
            ! A.onclick "window.location.href='/demo-account'"
            $ "Explore"
          H.button
            ! A.class_ "login-btn"
            ! A.onclick "window.location.href='/login'"
            $ "Log in"


-- Helper function to generate a feature card
featureCard :: Text -> Text -> Html
featureCard title description =
  H.div ! A.class_ "card" $ do
    H.h3 $ toHtml title
    H.p $ toHtml description
