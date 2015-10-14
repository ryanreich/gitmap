module Handlers (
  handleGitOp, when, whenQuitFailPass
  ) where

import Control.Monad hiding (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.HashMap.Strict as HashMap

import System.Directory
import System.Exit
import System.Process

import FileNames
import GitMapConfig

type ProcResult = ExceptT (Maybe (String, String)) IO (String, String)

noResult :: ProcResult
noResult = return ("", "")

quit :: ProcResult
quit = throwE Nothing

when :: Bool -> ProcResult -> ProcResult
when p x = if p then x else quit

whenQuitFailPass :: Either (Maybe a) a -> b -> (a -> b) -> (a -> b) -> b
whenQuitFailPass x y f g = case x of
  Left Nothing -> y
  Left z -> f z
  Right z -> g z

handleGitOp :: String -> [String] -> GitMapRepoSpec -> ProcResult

handleGitOp "clone" gitArgs repoSpec = do
  repoExists <- liftIO $ doesDirectoryExist $ gmrsName repoSpec
  when repoExists quit
  basicGitRun' "clone" [gmrsURL repoSpec] gitArgs repoSpec

handleGitOp gitOp gitArgs repoSpec = do
  lift $ setCurrentDirectory repoName
  (_, preOutput) <- preGitOp gitOp repoSpec
  (gitCmd, gitOutput) <- basicGitRun gitOp gitArgs repoSpec
  (_, postoutput) <- postGitOp gitOp repoSpec
  lift $ setCurrentDirectory ".."
  return $ (gitCmd, preOutput ++ gitOutput ++ postOutput)

preGitOp :: String -> GitMapRepoSpec -> ProcResult
preGitOp "pull" = gitCheckRemote
preGitOp "fetch" = gitCheckRemote
preGitOp "push" = gitCheckStatus
preGitOp "commit" = gitCheckStatus
preGitOp "stash" = gitCheckStatus
preGitOp _ = return noResult

postGitOp :: String -> GitMapRepoSpec -> ProcResult
postGitOp _ _ = return noResult

basicGitRun :: String -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun gitOp gitArgs = do
  basicGitRun' gitOp [] gitArgs

basicGitRun' :: String -> [String] -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun' gitOp gitArgs1 gitArgs2 repoSpec =
  gitRun gitOp:(gitArgs1 ++ gitRepoArgs gitOp repoSpec ++ gitArgs2) ""

gitCheckRemote :: ProcResult
gitCheckRemote = do
  gitRun ["remote", "update"] ""
  gitCheckStatus
  return noResult

gitCheckStatus :: ProcResult
gitCheckStatus = do
  statOutput <- gitRun ["status", "--porcelain"] ""
  when (null statOutput) $ throwE Nothing
  return noResult

gitRun :: [String] -> String -> ProcResult
gitRun args stdIn = do
  (exitCode, out, err) <-
    liftIO $ readProcssWithExitCode gitExecName args stdIn
  let output = out ++ err
  when (exitFailed exitCode) $ throwE $ Just output
  return (gitExecName ++ " " ++ intercalate " " args, output)

gitRepoArgs :: String -> GitMapRepoSpec -> [String]
gitRepoArgs gitOp repoSpec = HashMap.lookupDefault [] gitOp (gmrsGitArgs repoSpec)

exitFailed :: ExitCode -> Bool
exitFailed ExitSuccess = False
exitFailed _ = True
