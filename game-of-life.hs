module Main where
import Control.Concurrent
import System.Console.ANSI
import System.Random
import Data.ByteString.UTF8 (fromString)

width = 20

dot True = "●"
dot False = " "
unrow a = unwords (map dot a)
untable a = unlines (map unrow a)

mkrow i a = take width (drop (i * width) a)
mktable a = [mkrow i a | i<-[1..(width-1)]]
display s = untable (mktable s)

main = do
  clearScreen
  g <- newStdGen
  putStr "Seed: "
  putStrLn (head (words (show g)))
  let s = (take (width * width) (randoms g)) :: [Bool]
  putStr (display s)
  let sec = 1000000
  threadDelay (round (sec/4))
  main
