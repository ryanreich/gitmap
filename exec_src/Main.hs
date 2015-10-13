import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Yaml.Aeson as Yaml

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import GitMapConfig

import Debug.Trace

gitmapYaml :: String
gitmapYaml = "gitmap.yaml"

stackYaml :: String
stackYaml = "stack.yaml"

gitExecName :: String
gitExecName = "git"

main = do
  args <- getArgs
  readConfigData <- Yaml.decodeFileEither gitmapYaml
  let configData = either (error . Yaml.prettyPrintParseException) id readConfigData
            

  gitmapTime <- getModificationTime gitmapYaml
  stackYamlExists <- doesFileExist stackYaml
  updateStackYaml <-
    if stackYamlExists
    then do
      stackTime <- getModificationTime stackYaml
      return $ gitmapTime > stackTime
    else return True
  when updateStackYaml $ Yaml.encodeFile stackYaml $ gmcdStackYaml configData

  when (null args) exitSuccess

  let repoSpecs = sortBy (compare `on` gmrsName) $ gmcdRepoSpecs configData
  results <- forM repoSpecs $ \ (GitMapRepoSpec repoName repoURL repoGitArgs) ->
    let fullGitArgs = args ++ repoGitArgs
        repoPrefix = repoName ++ ":"
        errorPrefix = repoPrefix ++ " errors occurred:"
        clonePrefix = "Running `" ++ gitExecName ++ " clone " ++ repoURL ++ "`...\n"
        gitPrefix = "Running `" ++ gitExecName ++ " " ++
                    intercalate " " fullGitArgs ++ "`...\n"
    in eitherT (return . (== 0)) (const $ return True) $ do
      repoExists <- lift $ doesDirectoryExist repoName

      when (not repoExists) $ do
        (ex, ou, er) <-
          lift $ readProcessWithExitCode gitExecName ["clone", repoURL] ""
        let cloneOut = clonePrefix ++ ou ++ "\n"
            cloneErr = clonePrefix ++ er ++ "\n"
        when (exitFailed ex) $ do
          lift $ putStr $ errorPrefix ++ "\n" ++ cloneErr
          left 1
        lift $ putStr $ repoPrefix ++ "\n" ++ cloneOut

      when (head fullGitArgs == "clone") $ left 0

      lift $ setCurrentDirectory repoName
      (gitExit, gitOut,gitErr) <-
        lift $ readProcessWithExitCode gitExecName fullGitArgs ""
      lift $ setCurrentDirectory ".."

      when (exitFailed gitExit) $ do
        lift $ putStrLn $ errorPrefix ++ "\n" ++ gitPrefix ++ gitErr
        left 1

      lift $ putStrLn $ repoPrefix ++ "\n" ++ gitPrefix ++ gitOut
    
  when (not $ and results) $
    die $ "\nErrors occurred in some repositories. " ++
      "You may want to revert any successful changes."

exitFailed :: ExitCode -> Bool
exitFailed ExitSuccess = False
exitFailed _ = True
