{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Categorizer
import Database
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.HomePage
import HtmlGenerators.AllFilesPage
import HtmlGenerators.RefineSelectionPage
import Parsers
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Exception (try, SomeException)
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Map
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (FileInfo(..), tempFileBackEnd)
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as Web
import Network.HTTP.Client (Request(redactHeaders))
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding (decodeUtf8)
import Types




guessTransactions :: Text -> [Text]
guessTransactions txt =
    -- e.g. split on newlines, then filter or group:
    let linesAll = T.splitOn "\n" txt
    in linesAll  -- Or do something more advanced

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/" $ do
        content <- liftIO renderHomePage
        Web.html content

    get "/upload" $ do
        Web.html renderUploadPage

    post "/upload" $ do
        allFiles <- Web.Scotty.files
        case Prelude.lookup "pdfFile" allFiles of
            Nothing -> do
                Web.text "No file with field name 'pdfFile' was uploaded!"
            Just fileInfo -> do
                let uploadedBytes = fileContent fileInfo     
                let originalName  = decodeUtf8 $ fileName fileInfo        

                let tempFilePath = "/tmp/" <>  originalName
                liftIO $ B.writeFile (T.unpack tempFilePath) uploadedBytes

                extractedTextOrError <- liftIO $ try (extractTextFromPdf (T.unpack tempFilePath)) 
                                        :: ActionM (Either SomeException Text)
                case extractedTextOrError of
                    Left err -> do
                        Web.text $ "Failed to parse the PDF: " <> TL.pack (show err)
                    Right rawText -> do
                        liftIO $ insertPdfRecord "transactions.db"  originalName rawText
                        newPdfId <- liftIO $ insertPdfRecord "transactions.db" originalName rawText
                        redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show newPdfId))


    get "/adjust-transactions/:pdfId" $ do
        let dbPath = "transactions.db"
        pdfId <- pathParam "pdfId"
        (fileName, rawText) <- liftIO $ fetchPdfRecord dbPath pdfId  
        let guessedSegments = guessTransactions rawText        
        Web.html $ renderSliderPage pdfId fileName guessedSegments


    get "/transactions" $ do
        let dbPath = "transactions.db"
        filenames <- liftIO $ getAllFilenames dbPath
        Web.html $ renderAllFilesPage filenames

    get "/transactions/:filename" $ do

      let dbPath = "transactions.db"
      filename <- Web.Scotty.pathParam "filename"
      liftIO $ initializeDatabase dbPath  
      transactions <- liftIO $ getAllTransactions dbPath filename
      Web.html $ renderTransactionsPage (T.pack filename) transactions

    post "/update-category" $ do
      let dbPath = "transactions.db"
      tId     <- Web.Scotty.formParam "transactionId"   :: ActionM T.Text
      newCat  <- Web.Scotty.formParam "newCategory"     :: ActionM T.Text
      fileArg <- Web.Scotty.formParam "filename"        :: ActionM T.Text
      liftIO $ updateTransactionCategory dbPath (read $ T.unpack tId) newCat
      redirect $ TL.fromStrict ("/transactions/" <> fileArg)
