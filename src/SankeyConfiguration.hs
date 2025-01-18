{-# LANGUAGE OverloadedStrings #-}

module SankeyConfiguration
  ( generateSankeyConfig,
  )
where

import Control.Monad.IO.Unlift
import Data.Aeson hiding (Key)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, lookup, toList)
import Data.Text (Text, intercalate, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Database.Configurations
import Database.Models
import Database.Persist
import OpenAiUtils
import Types

formatEntitiesForInput :: Map (Entity TransactionSource) [Entity Category] -> [Text]
formatEntitiesForInput entities =
  concatMap formatPairs (toList entities)
  where
    formatPairs :: (Entity TransactionSource, [Entity Category]) -> [Text]
    formatPairs (Entity _ txSource, categories) =
      map
        ( \(Entity _ category) ->
            transactionSourceName txSource <> "-" <> categoryName category
        )
        categories

formatEntitiesForLinks ::
  Map (Entity TransactionSource) [Entity Category] ->
  [Entity TransactionSource] ->
  [Text]
formatEntitiesForLinks entities inputSources =
  [ transactionSourceName txSource <> "-" <> categoryName cat <> " -> " <> transactionSourceName sink
    | (Entity txId txSource, categories) <- toList entities,
      Entity _ cat <- categories,
      (Entity sinkId sink, _) <- toList entities,
      txId /= sinkId,
      Entity txId txSource `elem` inputSources
  ]

generateSankeyInputPrompt :: Map (Entity TransactionSource) [Entity Category] -> Text
generateSankeyInputPrompt entities =
  "Given the following (account, category) pairs generate a Sankey configuration."
    <> "First return account-category that are 'inputs', things like income. "
    <> "\n\nEntities:\n"
    <> intercalate "\n" (formatEntitiesForInput entities)

generateSankeyLinksPrompt :: [Text] -> Text
generateSankeyLinksPrompt formattedEntities =
  "Ok now we want to select any possible linkages within the acconts. Things like a bank account having a "
    <> "credit card payment category -> the credit card account"
    <> "each linkage should be a account-category that is source then return the account that is the sink"
    <> "\n\nEntities:\n"
    <> intercalate "\n" formattedEntities

generateSankeyInputSchema :: [Text] -> Value
generateSankeyInputSchema allowedValues =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= ("sankey_configuration_schema" :: Text),
            "strict" .= True,
            "schema"
              .= object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "inputs"
                          .= object
                            [ "type" .= ("array" :: Text),
                              "items"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "enum" .= allowedValues
                                  ]
                            ]
                      ],
                  "required" .= (["inputs"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

generateSankeyLinksSchema :: [Text] -> Value
generateSankeyLinksSchema allowedValues =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= ("sankey_links_schema" :: Text),
            "strict" .= True,
            "schema"
              .= object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "links"
                          .= object
                            [ "type" .= ("array" :: Text),
                              "items"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "enum" .= allowedValues
                                  ]
                            ]
                      ],
                  "required" .= (["links"] :: [Text]),
                  "additionalProperties" .= False
                ]
          ]
    ]

parseSankeyInputResponse :: B.ByteString -> Map Text (Entity TransactionSource) -> Map Text (Entity Category) -> IO (Maybe [(Entity TransactionSource, Entity Category)])
parseSankeyInputResponse responseBody txSourceMap categoryMap = do
  case decode responseBody of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right (Object obj) ->
          case parseMaybe (.: "inputs") obj of
            Just jsonArray -> do
              let parsedPairs = mapM (parseInputPair txSourceMap categoryMap) jsonArray
              return parsedPairs
            _ -> return Nothing
        Left err -> do
          putStrLn $ "Failed to decode JSON: " <> err
          return Nothing
    _ -> do
      putStrLn "Failed to decode OpenAI response."
      return Nothing

-- Convert "Bank-Income" -> (Entity TransactionSource, Entity Category)
parseInputPair :: Map Text (Entity TransactionSource) -> Map Text (Entity Category) -> Value -> Maybe (Entity TransactionSource, Entity Category)
parseInputPair txSourceMap categoryMap (String inputStr) =
  case splitOn "-" inputStr of
    [txName, catName] -> do
      txEntity <- Data.Map.lookup txName txSourceMap
      catEntity <- Data.Map.lookup catName categoryMap
      return (txEntity, catEntity)
    _ -> Nothing
parseInputPair _ _ _ = Nothing

parseSankeyLinksResponse :: B.ByteString -> Map Text (Entity TransactionSource) -> Map Text (Entity Category) -> IO (Maybe [((Entity TransactionSource, Entity Category), Entity TransactionSource)])
parseSankeyLinksResponse responseBody txSourceMap categoryMap = do
  case decode responseBody of
    Just ChatResponse {choices = (ChatChoice {message = ChatMessage {content = innerJson}} : _)} -> do
      case eitherDecode (B.fromStrict $ encodeUtf8 innerJson) of
        Right (Object obj) ->
          case parseMaybe (.: "links") obj of
            Just jsonArray -> do
              let parsedLinks = mapM (parseLinkPair txSourceMap categoryMap) jsonArray
              return parsedLinks
            _ -> return Nothing
        Left err -> do
          putStrLn $ "Failed to decode JSON: " <> err
          return Nothing
    _ -> do
      putStrLn "Failed to decode OpenAI response."
      return Nothing

-- Convert "Bank-Credit Card Payment -> Credit Card" into ((TransactionSource, Category), TransactionSource)
parseLinkPair :: Map Text (Entity TransactionSource) -> Map Text (Entity Category) -> Value -> Maybe ((Entity TransactionSource, Entity Category), Entity TransactionSource)
parseLinkPair txSourceMap categoryMap (String inputStr) =
  case splitOn " -> " inputStr of
    [sourceCat, sink] -> do
      case splitOn "-" sourceCat of
        [txName, catName] -> do
          txEntity <- Data.Map.lookup txName txSourceMap
          catEntity <- Data.Map.lookup catName categoryMap
          sinkEntity <- Data.Map.lookup sink txSourceMap
          return ((txEntity, catEntity), sinkEntity)
        _ -> Nothing
    _ -> Nothing
parseLinkPair _ _ _ = Nothing

-- | Fetches selected inputs from the LLM
fetchSankeyInputs ::
  Map (Entity TransactionSource) [Entity Category] ->
  Map Text (Entity TransactionSource) ->
  Map Text (Entity Category) ->
  IO (Maybe [(Entity TransactionSource, Entity Category)])
fetchSankeyInputs entities txSourceMap categoryMap = do
  let inputSchema = generateSankeyInputSchema $ formatEntitiesForInput entities
  let inputPrompt = generateSankeyInputPrompt entities
  let inputMessages = [ChatMessage {role = "user", content = inputPrompt}]

  responseInputs <- makeChatRequest inputSchema inputMessages
  case responseInputs of
    Left err -> do
      putStrLn $ "Error getting inputs: " ++ err
      return Nothing
    Right responseBodyInputs -> do
      print responseBodyInputs -- Debugging output
      parseSankeyInputResponse responseBodyInputs txSourceMap categoryMap

-- | Fetches possible linkages from the LLM, using selected inputs
fetchSankeyLinks ::
  Map (Entity TransactionSource) [Entity Category] ->
  [Entity TransactionSource] -> -- Selected input sources
  Map Text (Entity TransactionSource) ->
  Map Text (Entity Category) ->
  IO (Maybe [((Entity TransactionSource, Entity Category), Entity TransactionSource)])
fetchSankeyLinks entities selectedSources txSourceMap categoryMap = do
  let formattedEntities = formatEntitiesForLinks entities selectedSources
  let linkSchema = generateSankeyLinksSchema formattedEntities
  let linkPrompt = generateSankeyLinksPrompt formattedEntities
  let linkMessages = [ChatMessage {role = "user", content = linkPrompt}]

  print linkPrompt -- Debugging output
  responseLinks <- makeChatRequest linkSchema linkMessages
  case responseLinks of
    Left err -> do
      putStrLn $ "Error getting links: " ++ err
      return Nothing
    Right responseBodyLinks -> do
      print responseBodyLinks -- Debugging output
      parseSankeyLinksResponse responseBodyLinks txSourceMap categoryMap

persistSankeyConfig ::
  (MonadUnliftIO m) =>
  Entity User ->
  [(Entity TransactionSource, Entity Category)] -> -- Inputs
  [((Entity TransactionSource, Entity Category), Entity TransactionSource)] -> -- Linkages
  m (Key SankeyConfig)
persistSankeyConfig user inputPairs linkPairs = do
  let fullConfig =
        FullSankeyConfig
          { inputs = inputPairs,
            linkages = [(src, cat, sink) | ((src, cat), sink) <- linkPairs]
          }
  saveSankeyConfig user fullConfig

-- | Parent function that calls both helpers
generateSankeyConfig ::
  Map (Entity TransactionSource) [Entity Category] -> IO ()
generateSankeyConfig entities = do
  -- Step 1: Build lookup maps
  let txSourceMap = fromList [(transactionSourceName $ entityVal tx, tx) | (tx, _) <- toList entities]
  let categoryMap = fromList [(categoryName $ entityVal cat, cat) | (_, cats) <- toList entities, cat <- cats]

  -- Step 2: Get selected inputs
  inputPairs <- fetchSankeyInputs entities txSourceMap categoryMap
  case inputPairs of
    Just pairs -> do
      putStrLn "Parsed Sankey Inputs:"
      print pairs -- Debugging output

      -- Step 3: Fetch possible linkages
      linkPairs <- fetchSankeyLinks entities [ts | (ts, _) <- pairs] txSourceMap categoryMap
      case linkPairs of
        Just links -> do
          putStrLn "Parsed Sankey Links:"
          print links -- Debugging output
          -- Final Step: Combine inputPairs and linkPairs as needed
        Nothing -> putStrLn "Failed to parse Sankey links."
    Nothing -> putStrLn "Failed to parse Sankey inputs."