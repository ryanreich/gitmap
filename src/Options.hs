module Options (
  processArgs, options, Options(..)
  ) where

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
