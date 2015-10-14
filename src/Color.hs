module Color (
  putColored, successColor, errorColor, commandColor
  ) where

import System.Console.ANSI

resetColor :: IO ()
resetColor = setSGR [Reset]

successColor :: IO ()
successColor = setSGR [SetColor Foreground Vivid Cyan]

errorColor :: IO ()
errorColor = setSGR [SetColor Foreground Vivid Red]

infoColor :: IO ()
infoColor = setSGR [SetColor Foreground Vivid Yellow]

putColored :: IO () -> String -> IO ()
putColored_ op color text = 
  color
  op text
  resetColor

putColored = putColored_ putStrLn
putColored' = putColored putStr
