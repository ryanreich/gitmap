module Color (
  putError, putSuccess
  ) where

import System.Console.ANSI

resetColor :: IO ()
resetColor = setSGR [Reset]

successColor :: IO ()
successColor = setSGR [SetColor Foreground Vivid Yellow]

errorColor :: IO ()
errorColor = setSGR [SetColor Foreground Vivid Red]

commandColor :: IO ()
commandColor = setSGR [SetColor Foreground Vivid Cyan]

putColored :: IO () -> String -> String ->  String -> IO ()
putColored color prefix subtitle body = do
  color
  putStrLn prefix
  commandColor
  putStrLn subtitle
  resetColor
  putStrLn body

putError :: String -> String -> String -> IO ()
putError = putColored errorColor

putSuccess :: String -> String -> String -> IO ()
putSuccess = putColored successColor
