import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.Yaml.Aeson (Yaml)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import GitMapConfig

gitmapYaml :: String
gitmapYaml = "gitmap.yaml"

stackYaml :: String
stackYaml = "stack.yaml"

gitExecName :: String
gitExecName = "git"

main = do
  [execName, args] <- getArgs
  configData <- either (error . prettyPrintParseException) extractConfig <$>
            Yaml.decodeFileEither gitmapYaml

  gitmapTime <- getModificationTime gitmapYaml
  stackYamlExists <- doesFileExist stackYaml
  updateStackYaml <-
    if stackYamlExists
    then do
      stackTime <- getModificationTime stackYaml
      return gitmapTime > stackTime
    else return True
  when updateStackYaml $ Yaml.encodeFile stackYaml $ gmcdStackYaml config

  when (null args) exitSuccess

  when (head args == "clone") $
    die "Don't use 'gitmap clone'; missing repos are cloned automatically."
    
  let repoSpecs = sortBy (compare `on` gmrsName) $ gmcdRepoSpecs configData
  results <- forM repoSpecs $ \ (GitHashMapRepoSpec repoName repoURL repoGitArgs) ->
    let fullGitArgs = args ++ repoGitArgs
        repoPrefix = repoName ++ ":"
        errorPrefix = repoPrefix ++ " errors occurred:\n"
        clonePrefix = "Running " ++ gitExecName ++ " clone " ++ repoURL ++ "...\n"
        gitPrefix = "Running" ++ gitExecName ++ " " ++ fullGitArgs ++ "...\n"
    in eitherT (const False) (const True) $ do
      repoExists <- lift $ doesDirectoryExist repoName
      (cloneExit, cloneOut, cloneErr) <- lift $
        if repoExists
        then return (ExitSuccess, "", "")
        else do
          (ex, ou, er) <- readProcessWithExitCode gitExecName ["clone", repoURL] ""
          return (ex, clonePrefix ++ ou, clonePrefix ++ err)

      when (exitFailed cloneExit) $ do
        lift $ putStrLn $ errorPrefix ++ cloneErr
        left ()

      let finalPrefix = cloneOut ++ "\n" ++ gitPrefix ++ "\n"

      (gitExit, gitOut,gitErr) <- readProcessWithExitCode gitExecName fullGitArgs ""

      when (exitFailed gitExit) $ do
        lift $ putStrLn $ errorPrefix ++ finalPrefix ++ gitErr
        left ()

      lift $ putStrLn $ repoPrefix ++ finalPrefix ++ gitOut
    
  when (not . and results) $
    die "\nErrors occurred in some repositories. " ++
      "You may want to revert any successful changes."

exitFailed :: ExitCode -> Bool
exitFailed ExitSuccess = False
exitFailed _ = True
