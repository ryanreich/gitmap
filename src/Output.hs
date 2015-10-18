module Output (handleReport, printRemaining) where

import Control.Monad
import Data.Char
import Data.List
import System.Console.ANSI
import System.Exit

import GitMapConfig

handleReport :: Bool -> IO (Bool, GitMapRepoSpec, String, String) ->
                [GitMapRepoSpec] -> IO ()
handleReport status reportV repos = do
  (success, repoSpec, gitCmd, output) <- reportV
  let reposLeft = delete repoSpec repos
      repoNamesLeft = map gmrsName reposLeft
      newStatus = status && success
  printOutput success (gmrsName repoSpec) gitCmd output repoNamesLeft
  if (null reposLeft)
    then when (not newStatus) $
         die $ "\nErrors occurred in some repositories. " ++
         "You may want to revert any successful changes."
    else handleReport newStatus reportV reposLeft
         
printOutput :: Bool -> String -> String -> String -> [String] -> IO ()
printOutput success repoName gitCmd output reposLeft = do
  cursorUpLine $ 2 + length reposLeft --one removed, plus header
  setCursorColumn 0
  clearFromCursorToScreenEnd

  when (not $ null gitCmd) $ do
    setColor infoColor
    putStr $ repoName ++ ": "
    let (color, message) =
          if success
          then (successColor, "success")
          else (errorColor, "failed")
    setColor color
    putStrLn message

    setColor commandColor
    putStrLn $ "Ran `" ++ gitCmd ++ "`:"

    resetColor
    putStrLn $ dropWhileEnd isSpace $ dropWhile isSpace output

    putStrLn ""

  when (not $ null reposLeft) $ printRemaining reposLeft

printRemaining :: [String] -> IO ()
printRemaining reposLeft = do
  setColor infoColor
  putStrLn "Repositories remaining:"
  mapM_ putStrLn reposLeft  

resetColor :: IO ()
resetColor = setSGR [Reset]

successColor = Cyan
errorColor = Red
infoColor = Yellow
commandColor = Magenta

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]
