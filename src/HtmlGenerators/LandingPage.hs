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
    img,
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
    alt,
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
  H.div ! A.class_ " px-6 py-12 flex flex-col items-center text-center" $ do
    -- Banner
    H.div ! A.class_ "w-full max-w-2xl" $ do
      H.h1 ! A.class_ "text-4xl font-bold text-primary" $ "My Financé"
      H.h2 ! A.class_ "text-xl font-medium text-gray-800 mt-2" $ "Because you should love your finances"
      H.h2 ! A.class_ "text-lg text-gray-600 mt-2" $ "Simple personal finance for just $2/month or $20/year."

    H.div ! A.class_ "mt-8 flex justify-center" $ do
      H.img
        ! A.id "tilting-image"
        ! A.src "/landing.png"
        ! A.alt "Preview of the My Financé dashboard"
        ! A.class_ "w-full max-w-4xl rounded-lg shadow-2xl shadow-primary/50 transition-transform duration-300 ease-out"

    -- Call to Action
    H.div ! A.class_ "mt-12 text-center" $ do
      H.h3 ! A.class_ "text-xl font-semibold text-gray-900" $ "See it in action!"
      H.div ! A.class_ "mt-4 flex flex-col sm:flex-row gap-4 justify-center" $ do
        H.button
          ! A.class_ "primary-button"
          ! A.onclick "window.location.href='/demo-account'"
          $ "Try the demo"
        H.button
          ! A.class_ "secondary-button"
          ! A.onclick "window.location.href='/login'"
          $ "Sign up"

    -- Features Section
    H.div ! A.class_ "features-section mt-12" $ do
      H.div ! A.class_ "flex flex-col sm:flex-row gap-6 justify-center" $ do
        featureCard "Upload Transactions" "Automatically extract transactions from your bank statements."
        featureCard "Categorize Expenses" "Automatically categorize all your expenses."
        featureCard "See The Money Flow" "Understand where your money is actually going!"

-- Feature Card Component
featureCard :: Text -> Text -> Html
featureCard title description =
  H.div ! A.class_ "card bg-white shadow-md rounded-md p-6 w-full sm:w-1/3 text-center border border-primary transform translate-y-3 transition duration-200 ease-in-out" $ do
    H.h3 ! A.class_ "text-lg font-semibold text-primary" $ toHtml title
    H.p ! A.class_ "text-base text-gray-700 mt-2" $ toHtml description
