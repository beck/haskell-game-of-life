module Main where
import Control.Concurrent

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  threadDelay 3000000
  putStrLn "Goodbye"
