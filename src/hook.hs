{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main (main) where

import BasicPrelude
import qualified Data.ByteString.Lazy as BS
import Data.Machine.Runner (foldMapT)
import Data.Monoid (Last(..))
import Data.Aeson (encode, parseJSON)
import Data.Aeson.Types (Value(Object), parseEither)
import Data.NonEmpty.Set.Utils
import Data.TaskWarrior.Problem
import Data.TaskWarrior.Solution
import Data.TaskWarrior.Task
import System.IO (stdin)
import System.IO.Machine

main :: IO ()
main = do
  Last (Just affectedTaskV@(Object affectedTaskM)) <- foldMapT Last $ parseFrom "stdin" <$> sourceHandle byLine stdin
  let affectedTask = either (error . (("Could not parse stdin: ") ++)) id $ parseEither parseJSON affectedTaskV
      dd = due affectedTask
  problemsWithDependencies <- getDepsProblems dd $ depends affectedTask
  problemsWithDependents <- getDependentsProblems dd $ "depends:" ++ uuidValue (uuid affectedTask)
  let problems = problemsWithDependencies <> problemsWithDependents
  maybe (BS.putStr $ encode affectedTaskV) (solve dd affectedTaskM) $ nonEmptySet problems
