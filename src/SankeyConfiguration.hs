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
  "You are given the following (account, category) pairs for a user. we want to make a Sankey diagram that displays the flow of their money."
    <> "there may be a category that represents an external source of money, such as income"
    <> "First return an account-category list of these 'inputs', things like income."
    <> "also lets not assuming things like 'investments' are inherintly sources, they could also be purchasing investments"
    <> "\n\nEntities:\n"
    <> intercalate "\n" (formatEntitiesForInput entities)

generateSankeyLinksPrompt :: [Text] -> Text
generateSankeyLinksPrompt formattedEntities =
  "Ok now we want to select the linkages between acconts. Your selections here will create a intermediate node between the source and the sink of the graph."
    <> "This is a case where money in one account flows to another specific Account, such as a bank account having payments to a credit card. the payments would be  "
    <> "categorized as Bank-Credit Card Payment and those would 'flow' into the credit card node, (which will have it's own outputs)"
    <> "Only select linkages that make sense. We just want linkages that are logical and only one or none is fine."
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
      print responseBodyLinks
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

generateSankeyConfig ::
  (MonadUnliftIO m) =>
  Entity User ->
  Map (Entity TransactionSource) [Entity Category] ->
  m (Maybe FullSankeyConfig)
generateSankeyConfig user entities = do
  -- Step 1: Build lookup maps
  let txSourceMap = fromList [(transactionSourceName $ entityVal tx, tx) | (tx, _) <- toList entities]
  let categoryMap = fromList [(categoryName $ entityVal cat, cat) | (_, cats) <- toList entities, cat <- cats]

  -- Step 2: Get selected inputs
  inputPairs <- liftIO $ fetchSankeyInputs entities txSourceMap categoryMap
  case inputPairs of
    Just inputs -> do
      liftIO $ putStrLn "Parsed Sankey Inputs:"
      liftIO $ print inputs -- Debugging output

      -- Step 3: Fetch possible linkages
      linkPairs <- liftIO $ fetchSankeyLinks entities [tx | (tx, _) <- inputs] txSourceMap categoryMap
      case linkPairs of
        Just links -> do
          liftIO $ putStrLn "Parsed Sankey Links:"
          liftIO $ print links -- Debugging output
          return $ Just FullSankeyConfig {inputs = inputs, linkages = map flatten links}
        Nothing -> do
          liftIO $ putStrLn "Failed to parse Sankey links."
          return Nothing
    Nothing -> do
      liftIO $ putStrLn "Failed to parse Sankey inputs."
      return Nothing

flatten :: ((Entity TransactionSource, Entity Category), Entity TransactionSource) -> (Entity TransactionSource, Entity Category, Entity TransactionSource)
flatten ((a, b), c) = (a, b, c)