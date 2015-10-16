import Control.Concurrent

import Control.Monad
import Control.Monad.Trans.Except

import Data.Function
import Data.List
import qualified Data.Yaml.Aeson as Yaml

import System.Environment
import System.Exit
import System.Process

import Color
import FileNames
import GitMapConfig
import Handlers
import Options

main = do
  args <- getArgs
  let (opts, gitArgs) = processArgs args

  when (optShowHelp opts || null args) $ do
    execName <- getProgName
    putStrLn $ usage execName
    exitSuccess
  
  readConfigData <- Yaml.decodeFileEither gitmapYaml
  let configData = either (error . Yaml.prettyPrintParseException) id readConfigData
            
  when (optWriteStackYaml opts) $
    Yaml.encodeFile stackYaml $ gmcdStackYaml configData

  when (optWipe opts) $
    callProcess gitExecName ["clean", "-d", "-ff", "-e", "stack.yaml"]

  when (null gitArgs) exitSuccess

  let Just (gitOp, gitOpArgs) = uncons gitArgs
      repoSpecs = sortBy (compare `on` gmrsName) $ gmcdRepoSpecs configData

  mResults <- sequence $ map (const newEmptyMVar) repoSpecs
  sequence_ $ map forkIO $
    zipWith (handleRepo gitOp gitOpArgs opts) repoSpecs mResults
  results <- sequence $ map takeMVar mResults
  
  when (not $ and results) $
    die $ "\nErrors occurred in some repositories. " ++
      "You may want to revert any successful changes."

handleRepo :: String -> [String] -> Options ->
              GitMapRepoSpec -> MVar Bool -> IO ()
handleRepo gitOp gitOpArgs opts repoSpec status = do
  let repoName = gmrsName repoSpec
  runResult <- runExceptT $ handleGitOp gitOp gitOpArgs repoSpec
  putMVar status =<< whenQuitFailPass runResult
    (return True)
    (\(runCmd, runOutput) -> do
        printResult (putColored errorColor "failed") repoName runCmd runOutput
        return False)
    (\(runCmd, runOutput) -> do
        when (optShowOutput opts) $
          printResult (putColored successColor "success") repoName runCmd runOutput
        return True)

printResult :: IO () -> String -> String -> String -> IO ()
printResult showMessage repoName runCmd runOutput = do
  putColored' infoColor $ repoName ++ ": "
  showMessage
  when (not . null $ runCmd) $ putColored commandColor $ "Ran `" ++ runCmd ++ "`"
  putStrLn runOutput
