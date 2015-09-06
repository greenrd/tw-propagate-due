{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Data.TaskWarrior.Solution where

import BasicPrelude hiding (insert, lookup, null)
import Data.Aeson (encode, Value(String, Object))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.NonEmpty as NE
import qualified Data.NonEmpty.Set as NESet
import Data.NonEmpty.Set.Utils
import Data.Set (insert)
import Data.TaskWarrior.Problem
import Data.TaskWarrior.Task
import Data.Text (unpack)
import Data.Text.IO (hPutStrLn)
import System.Exit
import System.IO (hGetBuffering, hGetChar, hSetBuffering, BufferMode(NoBuffering), openFile, IOMode(ReadMode), stderr)
import System.Process

data Solution = BringForward (Either DueDate (NESet.T Task))
              | PutBack (Either DueDate (NESet.T Task))
              | Abort
              deriving (Eq, Ord)

describe :: Solution -> Text
describe (BringForward (Left dd))
  = "Bring forward this task's due date to " ++ show dd
describe (PutBack (Left dd))
  = "Put back this task's due date to " ++ show dd
describe (BringForward (Right tasks))
  = "Bring forward these tasks' due date: " ++ show (description <$> NESet.toAscList tasks)
describe (PutBack (Right tasks))
  = "Put back these tasks' due date: " ++ show (description <$> NESet.toAscList tasks)
describe Abort = "Abort current TaskWarrior command"

describeAll :: NESet.T Solution -> Text
describeAll solns = unlines . NE.toList $ describeFully <$> NESet.toAscList solns
  where
    describeFully solution = describe solution ++ " (" ++ show (key solution) ++ ")"

key :: Solution -> Char
key (BringForward _) = 'f'
key (PutBack _)      = 'b'
key Abort            = 'a'

solutions :: NESet.T Problem -> NESet.T Solution
solutions s = insertSimpleSolutions (soleMember s) $ NESet.singleton Abort
  where
    insertSimpleSolutions :: Maybe Problem -> NESet.T Solution -> NESet.T Solution
    insertSimpleSolutions Nothing = id -- Too hard, giving up
    insertSimpleSolutions (Just (DependenciesDueTooLate tasks)) =
       NESet.insert (PutBack . Left . NE.maximum . fmap due $ NESet.toAscList tasks) . insert (BringForward $ Right tasks) . NESet.flatten
    insertSimpleSolutions (Just (DependentsDueTooSoon tasks)) =
       NESet.insert (PutBack $ Right tasks) . insert (BringForward . Left . NE.minimum . fmap due $ NESet.toAscList tasks) . NESet.flatten

askUserIfAppropriate :: NESet.T Solution -> IO Solution
askUserIfAppropriate s = maybe askUser return $ soleMember s
  where
    askUser :: IO Solution
    askUser = do
      terminal <- openFile "/dev/tty" ReadMode
      hPutStrLn stderr $ "The following solutions are available:"
      hPutStrLn stderr $ describeAll s
      buffMode <- hGetBuffering terminal
      hSetBuffering terminal NoBuffering
      solutionChar <- hGetChar terminal
      hSetBuffering terminal buffMode
      return . fromMaybe Abort . find ((solutionChar ==) . key) . NE.toList $ NESet.toAscList s

-- | Changes the due date of the current task
changeCurrentDueDate :: DueDate -> HashMap Text Value -> IO ()
changeCurrentDueDate dd m = do
  BS8.putStrLn . encode . Object $ HashMap.insert "due" (String $ show dd) m
  putStrLn "Successfully changed due date of current task"

-- | Leave the current task unchanged and change the due date of some other task(s)
changeOtherDueDates :: DueDate -> NESet.T Task -> HashMap Text Value -> IO ()
changeOtherDueDates dd tasks m = do
  BS.putStr . encode $ Object m
  callProcess "task" [intercalate "," . map (unpack . uuidValue . uuid) . NE.toList $ NESet.toAscList tasks, "modify", "due:" ++ maybe "" (unpack . show) (dueDate dd)]

callProcess :: String -> [String] -> IO ()
callProcess cmd args = do
    exitCode <- system . intercalate " " $ cmd : args
    case exitCode of
      ExitSuccess   -> return ()
      ExitFailure _ -> fail "Process failed"

carryOut :: Solution -> HashMap Text Value -> DueDate -> IO ()
carryOut Abort _ _ = do
  putStrLn "{}"
  putStrLn "tw-propagate-due aborted"
  fail "Aborting"
carryOut (BringForward (Left dd)) m _ = changeCurrentDueDate dd m
carryOut (PutBack (Left dd)) m _ = changeCurrentDueDate dd m
carryOut (BringForward (Right tasks)) m dd = changeOtherDueDates dd tasks m
carryOut (PutBack (Right tasks)) m dd = changeOtherDueDates dd tasks m

solve :: DueDate -> HashMap Text Value -> NESet.T Problem -> IO ()
solve dd m problems = do
  hPutStrLn stderr $ "The following problem(s) were encountered:"
  hPutStrLn stderr $ explainAll problems
  let candidateSolutions = solutions problems
  chosenSolution <- askUserIfAppropriate candidateSolutions
  carryOut chosenSolution m dd
