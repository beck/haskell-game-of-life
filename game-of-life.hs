module Main where
import Control.Concurrent
import System.Console.ANSI


duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string


line 0 = return ()

line n = do
  line (n - 1)
  clearScreen
  threadDelay 100000
  putStrLn (duplicate "-" n)


main = do
  line 10
  clearScreen
  putStrLn "done"
