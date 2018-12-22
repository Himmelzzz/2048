module Funktionen
        where

import Data.Maybe
import Data.List
import Prelude

import System.Random
import Control.Monad.IO.Class
import System.IO.Unsafe

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

type Cell = Maybe Int
type Grid = [[Cell]]


printCell :: Cell -> String
printCell cell = case cell of
  Just n -> show n
  Nothing -> " "

mergeRow :: [Cell] -> [Cell]
mergeRow cells = case cells of
  Nothing:xs -> mergeRow xs
  x:Nothing:xs -> mergeRow (x:xs)
  (Just x):(Just y):xs -> if (x==y) then (Just (x*2)) : (mergeRow xs) else Just x : (mergeRow ((Just y):xs))
  xs -> xs

leftRow :: [Cell] -> [Cell]
leftRow cells = x ++ (replicate (4 - (length x)) Nothing)
  where x = mergeRow cells

leftGrid :: Grid -> Grid
leftGrid grid = map leftRow grid

scoreRow :: [Cell] -> Int
scoreRow [] = 0
scoreRow (x:xs) = case x of
  Nothing -> 0+scoreRow(xs)
  Just a -> a+scoreRow(xs)

scoreGrid :: Grid -> Int
scoreGrid grid = sum (map scoreRow grid)

blockCheck :: Grid -> Bool
blockCheck grid = (grid == leftGrid grid) && (grid == map reverse (leftGrid (map reverse grid))) && (grid == transpose (leftGrid (transpose grid))) && (grid == transpose (map reverse (leftGrid (map reverse (transpose grid)))))

singleBlockCheck :: Grid -> Bool
singleBlockCheck grid = (grid == leftGrid grid) || (grid == map reverse (leftGrid (map reverse grid))) || (grid == transpose (leftGrid (transpose grid))) || (grid == transpose (map reverse (leftGrid (map reverse (transpose grid)))))

fullCheck :: Grid -> Bool
fullCheck grid = (countNothing grid == 0)

createRandom :: Int -> Int
createRandom n = unsafePerformIO (getStdRandom (randomR(1,n)))


countRowNothing :: [Cell] -> Int
countRowNothing cells = case cells of
   [] -> 0
   Nothing:xs -> 1 + countRowNothing xs
   Just _:xs ->0 + countRowNothing xs

countNothing :: Grid -> Int
countNothing grid =sum $ map countRowNothing grid

createRandomCell :: Int -> Cell
createRandomCell n = if ((createRandom n)*37 `mod` 10) < 2 then Just 4
  else Just 2

insertRandomCell :: [Cell] -> Int -> Cell -> [Cell]
insertRandomCell cells n cell= case cells of
  Nothing:xs -> if (n-1/=0) then Nothing:insertRandomCell xs (n-1) cell else cell:xs
  x:xs -> x:insertRandomCell xs n cell
  [] -> []

insertRandomGrid :: Grid -> Int -> Cell -> Grid
insertRandomGrid grid 0 cell = grid
insertRandomGrid (x:xs) n cell= if (fullCheck (x:xs)) then x:xs
  else
  if (n>countRowNothing x)
    then x:insertRandomGrid xs (n-countRowNothing x) cell
    else insertRandomCell x n cell : xs

insertGrid :: Grid -> Grid
insertGrid grid =insertRandomGrid grid 5 cell1

data Game = Game
    { _grid :: Grid
    , _score :: Int
    , _done :: Bool
    } deriving (Eq, Show)

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Eq, Show)

initGame :: IO Game
initGame = do
    pure $
      Game{ _grid = [[Just 2, Just 2, Nothing, Nothing],
                     [Nothing, Nothing, Nothing, Nothing],
                     [Nothing, Nothing, Nothing, Nothing],
                     [Nothing, Nothing, Nothing, Nothing]]
          ,_score = 0
          ,_done = False
          }

cell1=Just 2
