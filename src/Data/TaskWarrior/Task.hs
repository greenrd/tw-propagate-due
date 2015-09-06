{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Data.TaskWarrior.Task where

import BasicPrelude hiding (show)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Prelude (Show(..))

-- | No need to parse - only ordering is required
newtype Date = Date Text deriving (Eq, Ord, Show, FromJSON)

newtype DueDate = DueDate { dueDate :: Maybe Date } deriving (Eq, FromJSON)

instance Show DueDate where
  show (DueDate Nothing) = "Never (no due date)"
  show (DueDate (Just dd)) = show dd

-- | Ordering that treats no date as a date infinitely far in the future. Suitable for due dates.
nothingLast :: Maybe Date -> Maybe Date -> Ordering
nothingLast Nothing Nothing = EQ
nothingLast (Just _) Nothing = LT
nothingLast Nothing (Just _) = GT
nothingLast (Just x) (Just y) = compare x y
                                        
instance Ord DueDate where
  compare (DueDate d1) (DueDate d2) = nothingLast d1 d2

newtype UUID = UUID { uuidValue :: Text } deriving (Eq, Ord, FromJSON, Show)

newtype UUIDList = UUIDList { uuidListValue :: Text } deriving (Eq, Ord, FromJSON, Show)

-- | We only include the fields of interest to this
-- application. ToJSON is not used; instead, we modify the Value.
data Task = Task { due         :: DueDate
                 , description :: Text
                 , depends     :: Maybe UUIDList
                 , uuid        :: UUID
                 } deriving (Eq, Ord, Show)

instance FromJSON Task where
  parseJSON (Object m) = 
    Task <$> m .:? "due" .!= DueDate Nothing <*> m .: "description" <*> m .:? "depends" <*> m .: "uuid"
  parseJSON v = typeMismatch "Object" v
