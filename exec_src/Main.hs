import Control.Concurrent
import Control.Monad

import Data.Function
import Data.List
import qualified Data.Yaml.Aeson as Yaml

import System.Environment
import System.Exit
import System.Process

import FileNames
import GitMapConfig
import Handlers
import Options
import Output

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
      quiet = optQuiet opts

  when (not quiet) $ printRemaining $ map gmrsName repoSpecs
  
  report <- newEmptyMVar
  mapM_ forkIO $
    map (handleRepo gitOp gitOpArgs (putMVar report)) repoSpecs
  handleReport True quiet (takeMVar report) repoSpecs
