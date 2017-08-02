{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Conduit                             ((.|))
import qualified Data.Conduit                             as C
import qualified Data.Conduit.Combinators                 as CC
import qualified GHC.Base

import           Protolude

import qualified Data.List                                as L
import           Options.Applicative                      hiding ((<>))
import qualified System.FilePath.Glob                     as G

import qualified JSONSchema.Draft4.Internal.Utils         as Utils
import           JSONSchema.Draft4.SchemaGeneration       (jsonToSchemaWithConfig,
                                                           unifySchemas)
import qualified JSONSchema.Draft4.SchemaGenerationConfig as SGC

data Input
  = FileInput [FilePath]
  | StandardInput
  deriving (Show)

data Options = Options
  { input            :: Input
  , generationConfig :: SGC.SchemaGenerationConfig
  } deriving (Show)

version :: Parser (a -> a)
version = infoOption "JSON to Schema 0.1.0" (long "version" <> short 'v' <> help "Prints version information.")

configParser :: Parser SGC.SchemaGenerationConfig
configParser =
  SGC.SchemaGenerationConfig <$>
  switch
    (long "type-arrays-as-tuples" <>
     help
       "When disabled (default), all objects in an array are assumed to have the \
              \ same type and are unified accordingly. When enabled, arrays are considered to\
              \ be tuples and each index will have its own type. For example, [14, false]\
              \ is either an array that can take integer or boolean objects (disabled) or a tuple \
              \ that is an integer in its first index and a boolean in its second.") <*>
  switch
    (long "seal-object-properties" <>
     help
       "Toggles additionalProperties for each object instance in the JSON schema. \
              \ When disabled (default), a JSON object instance that has more\
              \ properties than specified in the schema will continue to validate. \
              \ When enabled, a JSON object instance that has more properties \
              \ than specified will no longer validate.")

fileInputParser :: Parser Input
fileInputParser =
  FileInput <$>
  some
    (strOption
       (long "path" <> short 'p' <> metavar "TARGET" <>
        help
          "Can be specified multiple times. Each path can be a glob.\
                   \ All files matching these paths will be read and their JSONs \
                   \ will be combined."))

standardInputParser :: Parser Input
standardInputParser =
  flag'
    StandardInput
    (long "stdin" <> help "Reads JSON from stdin instead of a path, e.g. cat file.json | jsons-to-schema --stdin")

parser :: Parser Options
parser = Options <$> (fileInputParser <|> standardInputParser) <*> configParser

pInfo :: ParserInfo Options
pInfo =
  info
    (parser <**> helper <**> version)
    (fullDesc <> progDesc "A JSON Schema Draft 4 Generator from JSON instances." <> header "JSON to Schema")

globPaths :: [GHC.Base.String] -> IO [FilePath]
globPaths ss = (L.concat . fst) <$> G.globDir patterns ""
  where
    patterns = fmap G.compile ss

getSource (FileInput fps) = do
  paths <- globPaths fps
  putText $
    fromMaybe
      (panic "Could not find any files matching any of the globs provided")
      (if null paths
         then Nothing
         else Just "")
  return $ mapM_ CC.sourceFile paths
getSource StandardInput = return $ mapM_ CC.sourceHandle [stdin]

main :: IO ()
main = do
  options <- execParser pInfo
  let c = generationConfig options
  conduitSource <- getSource (input options)
  schema <-
    C.runConduitRes $ conduitSource .| CC.map (jsonToSchemaWithConfig c . Utils.parseValue) .| CC.foldl1 unifySchemas
  putText $ Utils.printSchema (fromMaybe (panic "Error unifying schemas.") schema)
