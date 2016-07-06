module Main where
import Control.Concurrent
import System.Console.ANSI
import Data.ByteString.UTF8 (fromString)

dot True = "●"
dot False = " "
unrow a = unwords (map dot a)
untable a = unlines (map unrow a)
display a = putStr (untable a)

main = do
  clearScreen
  let a = [[True, True, True], [True, True, True], [True, True, True]]
  display a
