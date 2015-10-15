module Handlers (
  handleGitOp, whenQuitFailPass
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.HashMap.Strict as HashMap
import Data.List

import System.Directory
import System.Exit
import System.IO.Error
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
  repoExists <- liftIO $ doesDirectoryExist $ gmrsName repoSpec
  when repoExists quit
  basicGitRun' "clone" [gmrsURL repoSpec] gitArgs repoSpec

handleGitOp gitOp gitArgs repoSpec = do
  let repoName = gmrsName repoSpec
      
  tryCD <- liftIO $ tryIOError $ setCurrentDirectory repoName
  either
    (const $ throwE $ Just ("", "Could not change to directory " ++
                                repoName ++ ". Did you run `gitmap clone`?"))
    (const noResult)
    tryCD

  inRepoResult <- liftIO $ runExceptT $ do
    (_, preOutput) <- preGitOp gitOp repoSpec
    lastModTime <- liftIO $ getModificationTime "."
    
    (gitCmd, gitOutput) <- basicGitRun gitOp gitArgs repoSpec
  
    currModTime <- liftIO $ getModificationTime "."
    (_, postOutput) <- postGitOp gitOp repoSpec

    return (gitCmd, preOutput ++ gitOutput ++ postOutput)
    
  liftIO $ setCurrentDirectory ".."

  whenQuitFailPass inRepoResult quit (throwE . Just) return


preGitOp :: String -> GitMapRepoSpec -> ProcResult
preGitOp "pull" = const gitCheckRemote
preGitOp "fetch" = const gitCheckRemote
preGitOp "push" = const gitCheckStatus
preGitOp "commit" = const gitCheckStatus
preGitOp "stash" = const gitCheckStatus
preGitOp _ = const noResult

postGitOp :: String -> GitMapRepoSpec -> ProcResult
postGitOp _ _ = noResult

basicGitRun :: String -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun gitOp gitArgs = do
  basicGitRun' gitOp [] gitArgs

basicGitRun' :: String -> [String] -> [String] -> GitMapRepoSpec -> ProcResult
basicGitRun' gitOp gitArgs1 gitArgs2 repoSpec =
  gitRun (gitOp:gitArgs1 ++ gitRepoArgs gitOp repoSpec ++ gitArgs2) ""

gitCheckRemote :: ProcResult
gitCheckRemote = do
  gitRun ["remote", "update"] ""
  gitCheckStatus

gitCheckStatus :: ProcResult
gitCheckStatus = do
  (_, statOutput) <- gitRun ["status", "--porcelain"] ""
  when (null statOutput) quit

gitRun :: [String] -> String -> ProcResult
gitRun args stdIn = do
  (exitCode, out, err) <-
    liftIO $ readProcessWithExitCode gitExecName args stdIn
  let output = out ++ err
      gitCmd = gitExecName ++ " " ++ intercalate " " args
  when (exitFailed exitCode) $ throwE $ Just (gitCmd, output)
  return (gitCmd, output)

gitRepoArgs :: String -> GitMapRepoSpec -> [String]
gitRepoArgs gitOp repoSpec = HashMap.lookupDefault [] gitOp (gmrsGitArgs repoSpec)

exitFailed :: ExitCode -> Bool
exitFailed ExitSuccess = False
exitFailed _ = True
