module Lib
  (executor)
  where

import Protolude

import qualified Data.ByteString as BS
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson.Encode.Pretty as AEEP

import JSONSchema.SchemaConverter (jsonToSchema)

executor :: IO ()
executor = do
    raw <- BS.readFile "./examples/sample.json"
    let json = AE.decode $ BSL.fromStrict raw :: Maybe AE.Value
    let prettySchemaize = AEEP.encodePretty . AE.toJSON . jsonToSchema

    putStrLn $ maybe "" prettySchemaize json
