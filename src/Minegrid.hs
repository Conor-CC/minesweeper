module Minegrid (
    setupBoard,
    Cell(CellInstance, hasMine, isClicked)
) where

import System.Random
import Control.Monad (replicateM)

data Cell = CellInstance {
                   hasMine :: Bool,
                   isClicked :: Bool
                 } deriving (Show, Eq)


-- Carries out board setup
setupBoard :: Int -> Int -> Int -> Float -> [[Cell]]
setupBoard 0 0 _ _ = [[]]
setupBoard n m seed difficulty = do
    makeBoard n m [] (createMinefield n m seed difficulty)

makeBoard :: Int -> Int -> [[Cell]] -> [Cell] -> [[Cell]]
makeBoard 0 _ xs mines = xs
makeBoard n m xs mines | n > 0 = makeBoard (n - 1) m ((fillRow (take m mines) []):xs) (drop m mines)

fillRow :: [Cell] -> [Cell] -> [Cell]
fillRow xs [] = xs
fillRow xs (val:mines) = fillRow (val:xs) mines


-- Generates T/F values in an list that maps to all cells in the board
createMinefield n m seed difficulty = do
  let size = n * m
  (buildMineGrid (take size (randoms (mkStdGen seed) :: [Float])) [] difficulty)

-- Generates mines as True and no mine as False. Amount of Mines depends on
-- difficulty value which is between 0 and 1.
buildMineGrid :: [Float] -> [Cell] -> Float -> [Cell]
buildMineGrid [] ys _ = ys
buildMineGrid (x:xs) ys difficulty | x <  difficulty = buildMineGrid xs ((CellInstance True  False):ys) difficulty
                                   | x >= difficulty = buildMineGrid xs ((CellInstance False False):ys) difficulty

getElement i j xs = do
                      let row = (xs !! i)
                      (row !! j)
