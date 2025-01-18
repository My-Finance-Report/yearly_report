{-# LANGUAGE OverloadedStrings #-}

module SankeyConfiguration
  ( generateSankeyConfig,
  )
where

import Data.Aeson hiding (Key)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, fromList, lookup, toList)
import Data.Text (Text, intercalate, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Database.Models
import Database.Persist
import OpenAiUtils
import Types

formatEntities :: Map (Entity TransactionSource) [Entity Category] -> [Text]
formatEntities entities =
  concatMap formatPairs (toList entities)
  where
    formatPairs :: (Entity TransactionSource, [Entity Category]) -> [Text]
    formatPairs (Entity _ txSource, categories) =
      map
        ( \(Entity _ category) ->
            transactionSourceName txSource <> "-" <> categoryName category
        )
        categories

-- \| Generate a prompt for the LLM to create the Sankey configuration
generateSankeyConfigPrompt :: Map (Entity TransactionSource) [Entity Category] -> Text
generateSankeyConfigPrompt entities =
  "Given the following (account, category) pairs generate a Sankey configuration."
    <> "First return (account,category) that are 'inputs', things like income. "
    <> "\n\nEntities:\n"
    <> intercalate "\n" (formatEntities entities)

generateSankeySchema :: [Text] -> Value
generateSankeySchema allowedValues =
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

generateSankeyConfig ::
  Map (Entity TransactionSource) [Entity Category] -> IO ()
generateSankeyConfig entities = do
  -- Step 1: Build lookup maps for TransactionSource and Category
  let txSourceMap = fromList [(transactionSourceName $ entityVal tx, tx) | (tx, _) <- toList entities]
  let categoryMap = fromList [(categoryName $ entityVal cat, cat) | (_, cats) <- toList entities, cat <- cats]

  -- Step 2: Generate schema and prompt
  let schema = generateSankeySchema $ formatEntities entities
  let prompt = generateSankeyConfigPrompt entities
  print prompt -- Debugging output
  let messages = [ChatMessage {role = "user", content = prompt}]

  -- Step 3: Make LLM API request
  response <- makeChatRequest schema messages
  case response of
    Left err -> putStrLn $ "Error: " ++ err
    Right responseBodyContent -> do
      print responseBodyContent -- Debugging output
      -- Step 4: Parse the response using lookup maps
      parsedData <- parseSankeyResponse responseBodyContent txSourceMap categoryMap
      case parsedData of
        Just pairs -> do
          putStrLn "Parsed Sankey Config:"
          print pairs -- Print the final parsed data
        Nothing -> putStrLn "Failed to parse the Sankey response."

parseSankeyResponse :: B.ByteString -> Map Text (Entity TransactionSource) -> Map Text (Entity Category) -> IO (Maybe [(Entity TransactionSource, Entity Category)])
parseSankeyResponse responseBody txSourceMap categoryMap = do
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
