{-# LANGUAGE FlexibleInstances #-}

module Main where
import Control.Concurrent
import System.Console.ANSI
import Data.ByteString.UTF8 (fromString)

dot True = "●"
dot False = " "

class Display a where
  display :: a -> String

instance Display Bool where
  display a = dot a

instance Display [Bool] where
  display a = unwords (map dot a)

instance Display [[Bool]] where
  display a = unlines (map display a)

main = do
  clearScreen
  let a = [[True, True, True], [True, True, True], [True, True, True]]
  putStr (display a)
