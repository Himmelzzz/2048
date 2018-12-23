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

handle :: Direction -> Grid -> Grid
handle d grid = case d of
  Funktionen.Up     -> transpose (leftGrid (transpose grid))
  Funktionen.Down   -> transpose (map reverse (leftGrid (map reverse (transpose grid))))
  Funktionen.Left   -> leftGrid grid
  Funktionen.Right  -> map reverse (leftGrid (map reverse grid))

move :: Direction -> Game -> Game
move dir g =
 if (_grid g == handle dir (_grid g))
    then
    Game {  _grid = _grid g
          , _score = _score g
          , _done = _done g
         }
    else
    Game {  _grid = newGrid
         , _score = (scoreGrid newGrid)
         , _done = (fullCheck newGrid && blockCheck newGrid)
         }
    where newGrid =  insertRandomGrid (handle dir (_grid g)) (createRandom (countNothing (handle dir (_grid g)))) (createRandomCell (countNothing (handle dir (_grid g))))


cell1=Just 2


--for AI

diffRow :: [Cell] -> Int
diffRow [] = 0
diffRow cells = case cells of
  Just a : Just b :xs -> abs (b-a) + diffRow (Just b : xs)
  Nothing : xs -> diffRow xs
  x : Nothing : xs -> diffRow (x:xs)
  [Just _] -> 0

diffRows :: Grid -> Int
diffRows grid = sum (map diffRow grid)

diffColumns :: Grid -> Int
diffColumns grid = sum (map diffRow $ transpose grid)

diffGrid :: Grid -> Int
diffGrid grid = diffRows grid + diffColumns grid


compareRow :: [Cell] -> Int
compareRow [] = 0
compareRow cells = case cells of
  Just a : Just b :xs -> if (a>b) then 1+compareRow (Just b : xs) else 0+compareRow (Just b : xs)
  Nothing : xs -> compareRow xs
  x : Nothing : xs -> compareRow (x:xs)
  [Just _] -> 0

compareRows :: Grid -> Int
compareRows grid = sum (map compareRow grid)

compareColumns :: Grid -> Int
compareColumns grid = sum (map compareRow $ transpose grid)

compareGrid :: Grid -> Int
compareGrid grid = compareRows grid + compareColumns grid

symbol :: Grid -> Double
symbol grid = fromIntegral (countNothing grid + (compareGrid grid)*15) / fromIntegral (diffGrid grid)

data Gd = Gd {
              grid_ :: Grid,
              direction_ :: Direction
              }deriving (Eq, Show)

maxfour :: Double->Double->Double->Double->Double
maxfour a b c d = max d $ max c $ max a b

issymbol :: Grid->Grid->Double
issymbol grid1 grid2 = if (grid1 == grid2) then 0 else symbol grid1

maxsymbol :: Grid -> Double
maxsymbol grid = maxfour (issymbol (handle Funktionen.Up grid) grid) (issymbol (handle Funktionen.Down grid) grid) (issymbol (handle Funktionen.Left grid) grid) (issymbol (handle Funktionen.Right grid) grid)

bestgrid :: Grid -> Grid
bestgrid grid = if (maxsymbol grid == (symbol $ handle Funktionen.Up grid)) then
  handle Funktionen.Up grid else if (maxsymbol grid == (symbol $ handle Funktionen.Down grid)) then
    handle Funktionen.Down grid else if (maxsymbol grid == (symbol $ handle Funktionen.Left grid)) then
      handle Funktionen.Left grid else handle Funktionen.Right grid

bestmove :: Game -> Game
bestmove g =
     Game {  _grid = newGrid
           , _score = (scoreGrid newGrid)
           , _done = (fullCheck newGrid && blockCheck newGrid)
           }
      where newGrid =  insertRandomGrid (bestgrid (_grid g)) (createRandom (countNothing (bestgrid (_grid g)))) (createRandomCell (countNothing (bestgrid (_grid g))))

--test
grid :: Grid
grid = [[Just 2, Just 4, Just 6, Nothing],
        [Nothing, Just 8, Just 3, Just 6],
        [Just 7, Nothing, Just 6, Nothing],
        [Just 2, Just 3, Just 6, Nothing]]

direction :: Direction
direction = Up
