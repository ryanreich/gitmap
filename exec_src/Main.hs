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
  let (opts, gitArgs) = processArgs args

  when (optShowHelp opts || null args) $ do
    execName <- getProgName
    putStrLn $ usageInfo (
      "Usage: " ++
      execName ++ " [options] [<git-action> [git-options]]\n" ++
      "where allowed options are:\n"
      ) options
    exitSuccess
  
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
         
  when (updateStackYaml && optWriteStackYaml opts) $
    Yaml.encodeFile stackYaml $ gmcdStackYaml configData

  when (null gitArgs) exitSuccess

  let repoSpecs = sortBy (compare `on` gmrsName) $ gmcdRepoSpecs configData
  results <- forM repoSpecs $ \ (GitMapRepoSpec repoName repoURL repoGitArgs) ->
    let gitOp = head gitArgs
        fullGitArgs = gitArgs ++ repoGitArgs
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

      when (gitOp == "clone") $ left 0

      lastModTime <- lift $ getModificationTime repoName
      lift $ setCurrentDirectory repoName
      (gitExit, gitOut,gitErr) <-
        lift $ readProcessWithExitCode gitExecName fullGitArgs ""
      lift $ setCurrentDirectory ".."
      currModTime <- lift $ getModificationTime repoName

      when (exitFailed gitExit) $ do
        lift $ putStrLn $ errorPrefix ++ "\n" ++ gitPrefix ++ gitOut ++ gitErr
        left 1

      when (currModTime > lastModTime || optShowOutput opts) $
        lift $ putStrLn $ repoPrefix ++ "\n" ++ gitPrefix ++ gitOut ++ gitErr
    
  when (not $ and results) $
    die $ "\nErrors occurred in some repositories. " ++
      "You may want to revert any successful changes."

exitFailed :: ExitCode -> Bool
exitFailed ExitSuccess = False
exitFailed _ = True

data Options =
  Options
  {
    optShowHelp :: Bool,
    optWriteStackYaml :: Bool,
    optShowOutput :: Bool
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg $ \opts -> opts{optShowHelp = True})
  "display usage",
  Option "y" ["write-stack-yaml"] (NoArg $ \opts -> opts{optWriteStackYaml = True})
  "write 'stack.yaml' based on 'gitmap.yaml'",
  Option "s" ["show-output"] (NoArg $ \opts -> opts{optShowOutput = True})
  "always show output, even if nothing changed"
  ]

processArgs :: [String] -> (Options, [String])
processArgs args = (opts, drop nArgs args)
  where (opts, nArgs) = makeOptions $ getOpt' RequireOrder options args

makeOptions :: ([Options -> Options], [String], [String], [String]) ->
               (Options, Int)
makeOptions (optOps, others, [], []) =
  (foldr (.) id optOps (Options False False False), length optOps)
makeOptions (_, _, nonOps, []) = error $ nonOpsError nonOps
makeOptions (_, _, [], errors) = error $ errorsError errors
makeOptions (_, _, nonOps, errors) =
  error $ nonOpsError nonOps ++ "\n" ++ errorsError errors

nonOpsError :: [String] -> String
nonOpsError nonOps =
  "Unrecognized options:\n" ++ intercalate ", " nonOps ++ "\n"

errorsError :: [String] -> String
errorsError errors =
  "Error(s) in processing options:\n" ++ intercalate "\n" errors ++ "\n"

