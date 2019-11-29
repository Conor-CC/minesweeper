module MinegridGame (
  update,
  checkWin,
  forceClearById,
  getNeigbourooneys
) where

import           Data.List (elemIndex)
import           Minegrid

-- If the length of a list of cells that are not clicked and dont have
-- mines is less than or equal to 0 than the game has been won
checkWin board = do
        let list = concat board
        let res = length $ filter (\x -> isClicked x == False && hasMine x == False) list
        if (res > 0)
          then False
          else True

-- See function immeadiately below for explanation
update :: [[Cell]] -> Cell -> [[Cell]]
update board cell = do
        updateCells board cell

-- Updates and reveals cell when clicked. If a cells proximity is 0,
-- all of the cells neighbouring it are revealed and updated also.
-- This process repeats until a proximity that is not 0 is encountered.
-- Note: "Updated" means (isClicked == True)
updateCells :: [[Cell]] -> Cell -> [[Cell]]
updateCells b cell = do
        let safeNeighbours = filter (\x -> cId x >= 0 && isClicked x == False) (getNeigbourooneys (pos cell) b)
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
updateByIdRow (x:xs) cell | (cId cell == cId x) == True = ((CellInstance False True (proximity x) (cId x) (pos x)):(updateByIdRow xs cell))
                          | otherwise = (x:(updateByIdRow xs cell))

-- This function along with its helpers are crucial for making sure the
-- first click of the game clears the clicked cell of mines as well as
-- all cells around it. While doing this, the oppurtunity is taken to
-- generate the proximity values for any non-mine cells so that time is
-- not wasted generating these for every subsequent click.
forceClearById :: [[Cell]] -> Cell -> [[Cell]]
forceClearById board cell = do
              -- Next two lines necessary as board is not properly gennerated
              -- until first click
              let cellPosition = findPos board cell
              let neighbouringCells = filter (\x -> cId x >= 0) (getNeigbourooneys cellPosition board)
              let newCell = CellInstance (hasMine cell) (isClicked cell) (proximity cell) (cId cell) (pos cell)
              -- force clear the selected cells and update all proximities
              let ids = map (\x -> cId x) (neighbouringCells)
              let forceCleared = forceClearByIdH board ((cId cell):ids) board
              setProximities forceCleared forceCleared

forceClearByIdH :: [[Cell]] -> [Int] -> [[Cell]] -> [[Cell]]
forceClearByIdH xs@[] _ _   = xs
forceClearByIdH (x:xs) ids b = ((forceClearCell x ids b):(forceClearByIdH xs ids b))

forceClearCell :: [Cell] -> [Int] -> [[Cell]] -> [Cell]
forceClearCell xs@[] _ _ = xs
forceClearCell (x:xs) ids b | elem (cId x) ids == True  = ((CellInstance False False (proximity x) (cId x) (pos x)):forceClearCell xs ids b)
                            | otherwise = (x:(forceClearCell xs ids b))

-- Returns the position of the clicked mine
findPos :: [[Cell]] -> Cell -> (Int, Int)
findPos board cell = iter board cell ((length board) - 1)

-- Get cell based on i j coordinates from the backend board.
-- If index out of bounds returns a cell with cID=-1 and hasMine = False
getCell :: Int -> Int -> [[Cell]] -> Int -> Cell
getCell i j xs lim | ((i > ((length xs) - 1)) || (j > (lim - 1))) = (CellInstance False False (-1) (-1) (-1, -1))
getCell i j xs lim | ((i < 0) || (j < 0)) = (CellInstance False False (-1) (-1) (-1, -1))
getCell i j xs lim = do
               let row = (xs !! ((lim - 1) - i))
               (row !! (((length row) - 1) - j))

-- A bit of a dirty function but the reasoning behind it should be obvious
-- Gets the neighbouring cells of a given cell
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

-- Sets all of the proximities on the board after the first click of the grid
-- only gets executed ONCE!!
setProximities xs b = setProximitiesH xs b

setProximitiesH :: [[Cell]] -> [[Cell]] -> [[Cell]]
setProximitiesH [] _     = []
setProximitiesH (x:xs) b = ((setProximitiesRowH x b):(setProximitiesH xs b))

setProximitiesRowH :: [Cell] -> [[Cell]] -> [Cell]
setProximitiesRowH [] b = []
setProximitiesRowH (x:xs) b = do
                if (hasMine x)
                  then do
                    let prox = (-1)
                    let newCell = CellInstance (hasMine x) (isClicked x) (prox) (cId x) (pos x)
                    (newCell:(setProximitiesRowH xs b))
                  else do
                    let position = findPos b x
                    let neighbouringCells = filter (\x -> cId x >= 0) (getNeigbourooneys position b)
                    let prox = (length $ filter (\x -> hasMine x) neighbouringCells)
                    let newCell = CellInstance (hasMine x) (isClicked x) (prox) (cId x) position
                    (newCell:(setProximitiesRowH xs b))

-- Returns positon of cell as (x, y) tuple
iter :: [[Cell]] -> Cell -> Int -> (Int, Int)
iter [] _ n = (n, -1)
iter (x:xs) mup n = case (elemIndex mup x) of
                 Just a  -> (n, (((length x) - a) - 1))
                 Nothing -> iter xs mup (n - 1)
