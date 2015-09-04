{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances,
GeneralizedNewtypeDeriving #-}
module Main (main) where

import BasicPrelude hiding (lookup)
import Data.ByteString hiding (putStrLn)
import Data.HashMap.Strict (lookup)
import Data.Machine
import Data.Aeson
import Data.Aeson.Types (parseEither, typeMismatch)
import Data.Text
import System.IO hiding (putStrLn)
import System.IO.Machine

-- | No need to parse - only ordering is required
newtype Date = Date Text deriving (Eq, Ord, Show, FromJSON)

newtype UUIDList = UUIDList Text deriving (Eq, FromJSON)

-- | We only include the fields of interest to this
-- application. ToJSON is not used; instead, we modify the Value.
data Task = Task { due     :: Maybe Date
                 , depends :: UUIDList
                 }

noDeps = UUIDList ""

instance FromJSON Task where
  parseJSON (Object m) = Task <$> m .:? "due" <*> m .:? "depends" .!= noDeps
  parseJSON v = typeMismatch "Object" v

parseFrom :: FromJSON a => String -> ByteString -> a
parseFrom desc bs = either (error . (("Could not parse " ++ desc ++ ": ") ++)) id $ eitherDecodeStrict bs

buildMachine :: MachineT IO k ()
buildMachine = construct $ do
  affectedTaskV <- deconstruct $ Left . parseFrom "stdin" <$> sourceHandle byLine stdin
  let affectedTask = either (error . (("Could not parse stdin: ") ++)) id $ parseEither parseJSON affectedTaskV
  liftIO . putStrLn . show $ due affectedTask

main :: IO ()
main = runT_ buildMachine
