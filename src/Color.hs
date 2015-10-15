module Color (
  putColored, putColored', successColor, errorColor, infoColor, commandColor
  ) where

import System.Console.ANSI

resetColor :: IO ()
resetColor = setSGR [Reset]

successColor = Cyan
errorColor = Red
infoColor = Yellow
commandColor = Magenta

putColored_ :: (String -> IO ()) -> Color -> String -> IO ()
putColored_ op color text = do
  setSGR [SetColor Foreground Vivid color]
  op text
  resetColor

putColored = putColored_ putStrLn
putColored' = putColored_ putStr
