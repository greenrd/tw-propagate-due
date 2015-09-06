{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Data.TaskWarrior.Problem where

import BasicPrelude
import Data.Aeson
import Data.Machine
import Data.Machine.Runner (foldMapT)
import qualified Data.NonEmpty as NE
import qualified Data.NonEmpty.Set as NESet
import Data.NonEmpty.Set.Utils
import Data.Set (singleton)
import Data.TaskWarrior.Task
import Data.Text (unpack)
import System.IO hiding (putStrLn)
import System.IO.Machine
import System.Process

data Problem = DependenciesDueTooLate (NESet.T Task)
             | DependentsDueTooSoon (NESet.T Task)
             deriving (Eq, Ord)

explain :: Problem -> Text
explain (DependenciesDueTooLate tasks) = 
  "These dependencies of this task are due after this task is: " ++ show (NESet.toAscList tasks)
explain (DependentsDueTooSoon tasks) =
  "These dependents of this task are due before this task is: " ++ show (NESet.toAscList tasks)

explainAll :: NESet.T Problem -> Text
explainAll problems = unlines . NE.toList $ explain <$> NESet.toAscList problems

parseFrom :: FromJSON a => String -> ByteString -> a
parseFrom desc bs = either (error . (("Could not parse " ++ desc ++ ": ") ++)) id $ eitherDecodeStrict bs

-- | Invokes "task export" and returns a handle to its output stream
taskExport :: Text -> IO Handle
taskExport twFilter = do
  (_, Just hOut, _, _) <- createProcess (proc "task" [unpack twFilter, "export"]) { std_out = CreatePipe }
  return hOut

getDepsProblems :: DueDate -> Maybe UUIDList -> IO (Set Problem)
getDepsProblems (DueDate Nothing) _                       = return mempty -- there cannot be any problems
getDepsProblems _                 Nothing                 = return mempty -- there are no dependencies
getDepsProblems dd                (Just (UUIDList uuids)) = do
  dependenciesH <- taskExport uuids
  badDeps <- foldMapT singleton $ filtered ((> dd) . due) <~ (parseFrom "task export" <$> sourceHandle byLine dependenciesH) 
  return . maybe mempty (singleton . DependenciesDueTooLate) $ nonEmptySet badDeps

getDependentsProblems :: DueDate -> Text -> IO (Set Problem)
getDependentsProblems dd twFilter = do
  dependentsH <- taskExport twFilter
  badDependents <- foldMapT singleton $ filtered ((< dd) . due) <~ (parseFrom "task export" <$> sourceHandle byLine dependentsH)
  return . maybe mempty (singleton . DependentsDueTooSoon) $ nonEmptySet badDependents

