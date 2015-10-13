import Control.Monad

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

main = do
  [execName, args] <- getArgs
  let (opts, actionM, nArgs) = makeOptions $ getOpts' RequireOrder options args
  when (optShowHelp opts) $
    putStrLn $ usageInfo (
      "Usage: " ++ execName ++ ": " ++
      execName ++ " [options] [<git-action> [git-options]]\n" ++
      "where allowed options are:\n"
      ) options
  configData <- either (error . prettyPrintParseException) extractConfig <$>
            Yaml.decodeFileEither "gitmap.yaml"
  when (optWriteStackYaml) $
    Yaml.encodeFile "stack.yaml" $ gmcdStackYaml configData
  when (isNothing actionM) $
    exitWith ExitSuccess
    
  let gitArgs = drop nArgs args
      gitExecName = "git"
      repoSpecs = sortBy (compare `on` gmrsName) $ gmcdRepoSpecs configData
      (callBeforeExec, afterExec) =
        if (actionM == "clone")
        then (const mempty, mempty)
        else (\x -> setCurrentDirectory x, setCurrentDirectory "..")

  results <- tryGitMap gitExecName gitArgs
             callBeforeExec afterExec repoSpecs

  map $ either
    (\(name, err) -> hPutStrLn stderr name ++ ": error occurred\n" ++ err ++ "\n")
    (\(name, out) -> putStrLn name ++ ":\n" ++ out ++ "\n")
    results
  hPutStrLn stderr
    "Errors occurred in some repositories: \n" ++
    "You may want to revert any successful changes."

  when (isLeft results) $
    exitWith $ ExitFailure 1

data Options =
  Options
  {
    optShowHelp :: Bool,
    optWriteStackYaml :: Bool
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg \opts -> opts{optShowHelp = True})
  "display usage",
  Option "y" ["write-stack-yaml"] (NoArg \opts -> opts{optWriteStackYaml = True})
  "write 'stack.yaml' based on 'gitmap.yaml'"
  ]

makeOptions :: ([Options -> Options], [String], [String], [String]) ->
               (Options, Maybe String)
makeOptions (optOps, others, [], []) =
  (foldr ($) optOps (Options False False),
   fmap fst $ uncons others,
   length optOps)
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

tryGitMap :: String -> [String] -> (String -> IO ()) -> IO () ->
             [GitHashMapRepoSpec] -> IO ()
tryGitMap gitExecName gitArgs callBeforeExec afterExec repoSpecs =
  sequence $ forM repoSpecs $ \ (GitHashMapRepoSpec repoName repoGitArgs) ->
    let fullGitArgs = gitArgs ++ repoGitArgs
    in do
      callBeforeExec repoName
      (exitCode,gitOut,gitErr) <- readProcessWithExitCode gitExecName fullGitArgs ""
      callAfterExec
      case exitCode of
        ExitSuccess -> return $ Right (repoName, gitOut)
        _ -> return $ Left (repoName, gitErr)
