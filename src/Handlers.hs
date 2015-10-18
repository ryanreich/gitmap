{-# LANGUAGE TupleSections #-}

module Handlers (handleRepo) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Data.HashMap.Strict as HashMap

import System.Directory
import System.Exit
import System.Process

import FileNames
import GitMapConfig

handleRepo :: String -> [String] -> Bool ->
              ((Bool, GitMapRepoSpec, String, String) -> IO ()) ->
              GitMapRepoSpec -> IO ()
handleRepo gitOp gitOpArgs quiet report repoSpec = do
  runResult <- runExceptT $ do
    result <- handleGitOp gitOp gitOpArgs repoSpec
    when (quiet) quit
    return result
  report $ whenQuitFailPass runResult
    (True, repoSpec, "", "")
    (uncurry (False, repoSpec, ,))
    (uncurry (True,  repoSpec, ,))

handleGitOp :: String -> [String] -> GitMapRepoSpec -> ProcResult
handleGitOp gitOp gitArgs repoSpec = do
  (_, preOutput) <- preGitOp gitOp repoSpec
  (gitCmd, gitOutput) <- doGitOp gitOp gitArgs repoSpec
  (_, postOutput) <- postGitOp gitOp repoSpec

  return (gitCmd, preOutput ++ gitOutput ++ postOutput)

doGitOp :: String -> [String] -> GitMapRepoSpec -> ProcResult
doGitOp "clone" gitArgs repoSpec =
  basicGitRun' "." "clone" [gmrsURL repoSpec] gitArgs repoSpec
doGitOp gitOp gitArgs repoSpec =
  basicGitRun (gmrsName repoSpec) gitOp gitArgs repoSpec

preGitOp :: String -> GitMapRepoSpec -> ProcResult
preGitOp gitOp repoSpec =
  let repoName = gmrsName repoSpec
      repoOp = case gitOp of
        "clone" -> gitSkipDir
        "pull" -> gitCheckRemote
        "fetch" -> gitCheckRemote
        "push" -> gitCheckStatus
        "commit" -> gitCheckStatus
        "stash" -> gitCheckStatus
        _ -> const noResult
  in repoOp repoName

gitSkipDir :: String -> ProcResult
gitSkipDir dir = do
  repoExists <- liftIO $ doesDirectoryExist dir
  when repoExists quit

gitCheckRemote :: String -> ProcResult
gitCheckRemote dir = do
  gitRun dir ["remote", "update"] ""
  gitCheckStatus dir

gitCheckStatus :: String -> ProcResult
gitCheckStatus dir = do
  (_, statOutput) <- gitRun dir ["status", "--porcelain"] ""
  when (null statOutput) quit

postGitOp :: String -> GitMapRepoSpec -> ProcResult
postGitOp _ _ = noResult

basicGitRun :: String -> String -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun dir gitOp gitArgs = do
  basicGitRun' dir gitOp [] gitArgs

basicGitRun' :: String -> String -> [String] -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun' dir gitOp gitArgs1 gitArgs2 repoSpec =
  gitRun dir (gitOp:gitArgs1 ++ gitRepoArgs gitOp repoSpec ++ gitArgs2) ""

gitRepoArgs :: String -> GitMapRepoSpec -> [String]
gitRepoArgs gitOp repoSpec = HashMap.lookupDefault [] gitOp (gmrsGitArgs repoSpec)

gitRun :: String -> [String] -> String -> ProcResult
gitRun dir args stdIn = do
  (exitCode, out, err) <-
    liftIO $ readProcessWithExitCode gitExecName (["-C", dir] ++ args) stdIn
    
  let output = out ++ err
      gitCmd = showCommandForUser gitExecName args
      returnOp = case exitCode of
        ExitSuccess -> return
        _ -> throwE . Just
        
  returnOp (gitCmd, output)

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
