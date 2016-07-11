module Main where
import Control.Concurrent
import System.Console.ANSI
import System.Random
import Debug.Trace
import Data.ByteString.UTF8 (fromString)


instance Show (a -> b) where
  show _ = "<function>"


width = 20

-- convert state to IO

dot True = "●"
dot False = " "
unrow a = unwords (map dot a)
untable a = unlines (map unrow a)

mkrow i a = take width (drop (i * width) a)
mktable a = [mkrow i a | i<-[0..(width)]]
display list = untable (mktable list)


-- generate new state

top i = i - width
left i = i-1
right i = i + 1
bot i = i + width

istop i = i < width
isbot i = i > ((width * width) - width - 1)
isleft i = (mod i width) == 0
isright i = (mod i width) == (width - 1)

neighborLookups "bottom-edge" = [top.right, top, top.left, right, left]
neighborLookups "bottom-left-corner" = [top, top.right, right]
neighborLookups "bottom-right-corner" = [top.left, top, left]
neighborLookups "left-edge" = [top, top.right, right, bot.right, bot]
neighborLookups "middle" = [top.left, top, top.right, right, bot.right, bot, bot.left, left]
neighborLookups "right-edge" = [top.left, top, bot, bot.left, left]
neighborLookups "top-edge" = [right, bot.right, bot, bot.left, left]
neighborLookups "top-left-corner" = [right, bot.right, bot]
neighborLookups "top-right-corner" = [bot, bot.left, left]

whereis i
  | (istop i) && (isleft i) = "top-left-corner"
  | (istop i) && (isright i) = "top-right-corner"
  | (istop i) = "top-edge"
  | (isbot i) && (isleft i) = "bottom-left-corner"
  | (isbot i) && (isright i) = "bottom-right-corner"
  | (isbot i) = "bottom-edge"
  | (isleft i) = "left-edge"
  | (isright i) = "right-edge"
  | otherwise = "middle"

iToNeighborIndexes i = do
  let lookups = (neighborLookups (whereis i))
  map (\x -> (x i)) lookups


iToNeighborValues i life = do
  let idxs = iToNeighborIndexes i
  map (\x -> (life!!x)) idxs


countLivingNeighbors i life = do
  let bools = iToNeighborValues i life
  length (filter (\x -> x) bools)


-- core rules for Conway's Game of Life

isCellAlive i life
  | underPopulated = False
  | livesToNextGeneration = True
  | reproduction = True
  | otherwise = False -- over-population
  where
    livingNeighbors = countLivingNeighbors i life
    underPopulated =  livingNeighbors < 2
    isAlive = life!!i
    isDead = life!!i == False
    sweetSpot = (livingNeighbors == 2) || (livingNeighbors == 3)
    livesToNextGeneration = isAlive && sweetSpot
    reproduction = isDead && livingNeighbors == 3


-- Main

ageLife state = do
  map (\x -> (isCellAlive x state)) [0..(width * width - 1)]

gameOfLife state = do
  clearScreen
  putStr (display state)
  let sec = 1000000
  threadDelay (round (sec/8))
  gameOfLife (ageLife state)

mkfalse n = map (\x -> False) [1..n]

initLife g = do
  take (width * width) (randoms g) :: [Bool]


main = do
  randSeed <- newStdGen
  let seed = (head (words (show randSeed)))
  let g = (mkStdGen (read seed :: Int))
  let state = initLife g
  gameOfLife state
