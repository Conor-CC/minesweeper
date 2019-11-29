module Minegrid where


import           Control.Monad (replicateM)
import           Data.List     (elemIndex)
import           Debug.Trace
import           System.Random


data Cell = CellInstance {
                   hasMine    :: Bool,
                   isClicked  :: Bool,
                   proximity  :: Int,
                   cId        :: Int,
                   neighbours :: [Cell]
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
fillRow xs []          = xs
fillRow xs (val:mines) = fillRow (val:xs) mines

-- Generates T/F values in an list that maps to all cells in the board
createMinefield n m seed difficulty = do
    let size = n * m
    buildMineGrid (take size (randoms (mkStdGen seed) :: [Float])) [] difficulty 0

-- Generates mines as True and no mine as False. Amount of Mines depends on
-- difficulty value which is between 0 and 1.
buildMineGrid :: [Float] -> [Cell] -> Float -> Int -> [Cell]
buildMineGrid [] ys _ _ = ys
buildMineGrid (x:xs) ys difficulty n | x <  difficulty = buildMineGrid xs ((CellInstance True  False (-1) n []):ys) difficulty (n + 1)
                                     | x >= difficulty = buildMineGrid xs ((CellInstance False False (-1) n []):ys) difficulty (n + 1)


updateAndCheckWin :: [[Cell]] -> Cell -> [[Cell]]
updateAndCheckWin board cell = do
        updateCells board cell

updateCells :: [[Cell]] -> Cell -> [[Cell]]
updateCells b cell = do
        let safeNeighbours = filter (\x -> cId x >= 0 && isClicked x == False) (getNeigbourooneys (iter b cell ((length b) - 1)) b)
        let prox = proximity cell
        if (prox == 0)
          then do
              let current = updateById b cell
              foldl updateCells current safeNeighbours
          else
              updateById b cell

updateById :: [[Cell]] -> Cell -> [[Cell]]
updateById xs@[] _      = xs
updateById (x:xs) cells = ((updateByIdRow x cells):(updateById xs cells))

updateByIdRow :: [Cell] -> Cell -> [Cell]
updateByIdRow xs@[] _ = xs
updateByIdRow (x:xs) cell | (cId cell == cId x) == True = ((CellInstance False True (proximity x) (cId x) (neighbours x)):(updateByIdRow xs cell))
                          | otherwise = (x:(updateByIdRow xs cell))

forceClearById :: [[Cell]] -> Cell -> [[Cell]]
forceClearById board cell = do
              -- Next two lines necessary as board is not properly gennerated
              -- until first click
              let neighbouringCells = filter (\x -> cId x >= 0) (getNeigbourooneys (iter board cell ((length board) - 1)) board)
              let newCell = CellInstance (hasMine cell) (isClicked cell) (proximity cell) (cId cell) (neighbouringCells)
              -- force clear the selected cells and update all proximities
              let ids = map (\x -> cId x) (neighbouringCells)
              let forceCleared = forceClearByIdH board ((cId cell):ids) board
              let updatedNeighboursBoard = updateNeighbourLists forceCleared forceCleared
              updatedNeighboursBoard

forceClearByIdH :: [[Cell]] -> [Int] -> [[Cell]] -> [[Cell]]
forceClearByIdH xs@[] _ _   = xs
forceClearByIdH (x:xs) ids b = ((forceClearCell x ids b):(forceClearByIdH xs ids b))

forceClearCell :: [Cell] -> [Int] -> [[Cell]] -> [Cell]
forceClearCell xs@[] _ _ = xs
forceClearCell (x:xs) ids b | checkIfMatch (cId x) ids == True  = ((CellInstance False False (proximity x) (cId x) []):forceClearCell xs ids b)
                            | checkIfMatch (cId x) ids == False = (x:(forceClearCell xs ids b))

checkIfMatch :: Int -> [Int] -> Bool
checkIfMatch _ [] = False
checkIfMatch cellId (x:xs) | x == cellId = True
                           | otherwise = checkIfMatch cellId xs

-- Returns the position of the clicked mine
findPos :: [[Cell]] -> Cell -> (Int, Int)
findPos board cell = iter board cell ((length board) - 1)

-- Finds mine in backend board that matches with clicked mine.
-- Unique identifier cId in the CellInstance constructor makes
-- this easier.
iter :: [[Cell]] -> Cell -> Int -> (Int, Int)
iter [] _ n = (n, -1)
iter (x:xs) mup n = case (elemIndex mup x) of
                 Just a  -> (n, (((length x) - a) - 1))
                 Nothing -> iter xs mup (n - 1)

-- If hasMine == True, increase count by one. Otherwise by 0
countMines :: [Cell] -> Int
countMines []  = 0
countMines (x:xs)  | (hasMine x) == True = (1 + (countMines xs))
                   | (hasMine x) == False = (0 + (countMines xs))

-- Get cell based on i j coordinates from the backend board.
-- If index out of bounds returns a cell with cID=-1 and hasMine = False
getCell :: Int -> Int -> [[Cell]] -> Int -> Cell
getCell i j xs lim | ((i > ((length xs) - 1)) || (j > (lim - 1))) = (CellInstance False False (-1) (-1) [])
getCell i j xs lim | ((i < 0) || (j < 0)) = (CellInstance False False (-1) (-1) [])
getCell i j xs lim = do
               let row = (xs !! ((lim - 1) - i))
               (row !! (((length row) - 1) - j))

getNeigbourooneys (x, y) board = do
                       let a =  (getCell (x - 1) (y - 1) board (length board))
                       let b =  (getCell (x - 1) (y) board (length board))
                       let c =  (getCell (x - 1) (y + 1) board (length board))
                       -- Left and Right
                       let d =  (getCell (x) (y - 1) board (length board))
                       let e =  (getCell (x) (y + 1) board (length board))
                       -- Bottom Row
                       let f =  (getCell (x + 1) (y - 1) board (length board))
                       let g =  (getCell (x + 1) (y) board (length board))
                       let h =  (getCell (x + 1) (y + 1) board (length board))
                       (h:(g:(f:(e:(d:(c:(b:(a:[]))))))))

updateNeighbourLists xs b = updateNeighbourListsH xs b

updateNeighbourListsH :: [[Cell]] -> [[Cell]] -> [[Cell]]
updateNeighbourListsH [] _ = []
updateNeighbourListsH (x:xs) b = ((updateNeighbourListsRow x b):(updateNeighbourListsH xs b))

updateNeighbourListsRow :: [Cell] -> [[Cell]] -> [Cell]
updateNeighbourListsRow [] b = []
updateNeighbourListsRow (x:xs) b = do
                if (hasMine x)
                  then do
                    let prox = (-1)
                    let newCell = CellInstance (hasMine x) (isClicked x) (prox) (cId x) (neighbours x)
                    (newCell:(updateNeighbourListsRow xs b))
                  else do
                    let neighbouringCells = filter (\x -> cId x >= 0) (getNeigbourooneys (iter b x ((length b) - 1)) b)
                    let prox = (length $ filter (\x -> hasMine x) neighbouringCells)
                    let newCell = CellInstance (hasMine x) (isClicked x) (prox) (cId x) (neighbouringCells)
                    (newCell:(updateNeighbourListsRow xs b))
