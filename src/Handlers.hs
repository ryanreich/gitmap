module Handlers (
  handleGitOp, whenQuitFailPass
  ) where

import Control.Monad.IO.Class
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
when p x = if p then x else noResult

whenQuitFailPass :: Either (Maybe a) a -> b -> (a -> b) -> (a -> b) -> b
whenQuitFailPass x y f g = case x of
  Left Nothing -> y
  Left (Just z) -> f z
  Right z -> g z

handleGitOp :: String -> [String] -> GitMapRepoSpec -> ProcResult

handleGitOp "clone" gitArgs repoSpec = do
  let repoName = gmrsName repoSpec
  repoExists <- liftIO $ doesDirectoryExist repoName
  when repoExists quit
  basicGitRun' "." "clone" [gmrsURL repoSpec] gitArgs repoSpec

handleGitOp gitOp gitArgs repoSpec = do
  let repoName = gmrsName repoSpec

  (_, preOutput) <- preGitOp gitOp repoName repoSpec
  (gitCmd, gitOutput) <- basicGitRun repoName gitOp gitArgs repoSpec
  (_, postOutput) <- postGitOp gitOp repoName repoSpec

  return (gitCmd, preOutput ++ gitOutput ++ postOutput)

preGitOp :: String -> String -> GitMapRepoSpec -> ProcResult
preGitOp "pull" = const . gitCheckRemote
preGitOp "fetch" = const . gitCheckRemote
preGitOp "push" = const . gitCheckStatus
preGitOp "commit" = const . gitCheckStatus
preGitOp "stash" = const . gitCheckStatus
preGitOp _ = const . const noResult

postGitOp :: String -> String -> GitMapRepoSpec -> ProcResult
postGitOp _ _ = const noResult

basicGitRun :: String -> String -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun dir gitOp gitArgs = do
  basicGitRun' dir gitOp [] gitArgs

basicGitRun' :: String -> String -> [String] -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun' dir gitOp gitArgs1 gitArgs2 repoSpec =
  gitRun dir (gitOp:gitArgs1 ++ gitRepoArgs gitOp repoSpec ++ gitArgs2) ""

gitCheckRemote :: String -> ProcResult
gitCheckRemote dir = do
  gitRun dir ["remote", "update"] ""
  gitCheckStatus dir

gitCheckStatus :: String -> ProcResult
gitCheckStatus dir = do
  (_, statOutput) <- gitRun dir ["status", "--porcelain"] ""
  when (null statOutput) quit

gitRun :: String -> [String] -> String -> ProcResult
gitRun dir args stdIn = do
  (exitCode, out, err) <-
    liftIO $ readProcessWithExitCode gitExecName (["-C", dir] ++ args) stdIn
  let output = out ++ err
      gitCmd = showCommandForUser gitExecName args
  when (exitFailed exitCode) $ throwE $ Just (gitCmd, output)
  return (gitCmd, output)

gitRepoArgs :: String -> GitMapRepoSpec -> [String]
gitRepoArgs gitOp repoSpec = HashMap.lookupDefault [] gitOp (gmrsGitArgs repoSpec)

exitFailed :: ExitCode -> Bool
exitFailed ExitSuccess = False
exitFailed _ = True
