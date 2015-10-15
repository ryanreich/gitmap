import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.HashMap.Strict as HashMap
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Yaml.Aeson as Yaml

import System.Directory
import System.Environment
import System.Exit
import System.IO
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

  let Just (gitOp, gitOpArgs) = uncons gitArgs
      repoSpecs = sortBy (compare `on` gmrsName) $ gmcdRepoSpecs configData

  results <- forM repoSpecs $ \repoSpec -> do
    let repoName = gmrsName repoSpec
    runResult <- runExceptT $ handleGitOp gitOp gitOpArgs repoSpec
    whenQuitFailPass runResult
      (return True)
      (\(runCmd, runOutput) -> do
          putColored' infoColor $ repoName ++ ": "
          putColored errorColor "failed"
          when (not . null $ runCmd) $ putColored commandColor $ "Ran " ++ runCmd
          putStrLn runOutput
          return False)
      (\(runCmd, runOutput) -> do
          when (not (null runCmd) && optShowOutput opts) $ do
            putColored' infoColor $ repoName ++ ": "
            putColored successColor "success"
            putColored commandColor $ "Ran " ++ runCmd
            putStrLn runOutput
          return True)
    
  when (not $ and results) $
    die $ "\nErrors occurred in some repositories. " ++
      "You may want to revert any successful changes."



