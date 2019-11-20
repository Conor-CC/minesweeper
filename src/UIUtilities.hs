module UIUtilities
    (
      generateGrid
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Minegrid (Cell(CellInstance, hasMine, isClicked, cId))
import Data.List (elemIndex)
import System.Exit

-- generateGrid and generateRow create a frontend representation of [[Cell]]
generateGrid [] ys b w = ys
generateGrid (x:xs) ys b w = generateGrid xs ((generateRow x [] b w):ys) b w

-- genrateRow
generateRow [] ys b w = ys
generateRow (x:xs) ys b w = generateRow xs ((makeButton x b w):ys) b w

-- Creates a button to be used in the browser minesweeper game board
makeButton :: Cell -> [[Cell]] -> Window -> UI Element
makeButton cell board w = do
        -- Assign variables concerned with relevant cell
        let matchingCell = cell
        let mine = hasMine matchingCell
        let cellId = cId matchingCell

        -- Define baseline cell attributes
        button <- UI.button # set text "?"
                            # set (attr "cId") (show cellId)
                            # set style [("color", "#000000")]
                            # set (attr "oncontextmenu") ("return false;")

        -- Define events and their responses  based on cell type and state
        case (mine) of
            True -> (on UI.click button $ \_ -> do
                    element button # set text "M"
                                   # set style [("color", "#000000"), ("background-color", "#f44336")]
                    getBody w #+ [UI.h1 #+ [string "YOU LOST"]]
                    reset <- UI.button # set text "Reset?"
                    getBody w #+ [element reset])
            False -> (on UI.click button $ \_ -> do
                    let proximity = genProximity board matchingCell -- Find current position, see if neighbours have mines
                    element button # set text (show proximity))

        on UI.contextmenu button $ \_ -> do
                element button # set text "F"
        on UI.hover button $ \_ -> do
                element button # set style [("color", "#DC143C")]
        on UI.leave button $ \_ -> do
                element button # set style [("color", "#000000")]

        return button

--Finds position of the clicked non-mine cell, count how many mines are nearby
genProximity board cell = mineNeighbours (iter board cell ((length board) - 1)) board


mineNeighbours :: (Int, Int) -> [[Cell]] -> Int
mineNeighbours (x, y) board = do
                            -- Top Row
                            let a = getCell (x - 1) (y - 1) board (length board)
                            let b = getCell (x - 1) (y) board (length board)
                            let c = getCell (x - 1) (y + 1) board (length board)
                            -- Left and Right
                            let d = getCell (x) (y - 1) board (length board)
                            let e = getCell (x) (y + 1) board (length board)
                            -- Bottom Row
                            let f = getCell (x + 1) (y - 1) board (length board)
                            let g = getCell (x + 1) (y) board (length board)
                            let h = getCell (x + 1) (y + 1) board (length board)
                            -- Count how many mines present
                            countMines (h:(g:(f:(e:(d:(c:(b:(a:[]))))))))

-- Returns the position of the clicked mine
findPos :: [[Cell]] -> Cell -> (Int, Int)
findPos board cell = iter board cell ((length board) - 1)

-- Finds mine in backend board that matches with clicked mine.
-- Unique identifier cId in the CellInstance constructor makes
-- this easier.
iter :: [[Cell]] -> Cell -> Int -> (Int, Int)
iter [] _ n = (n, -1)
iter (x:xs) mup n = case (elemIndex mup x) of
                  Just a -> (n, (((length x) - a) - 1))
                  Nothing -> iter xs mup (n - 1)

-- If hasMine == True, increase count by one. Otherwise by 0
countMines :: [Cell] -> Int
countMines [] = 0
countMines (x:xs) | (hasMine x) == True = (1 + (countMines xs))
                  | (hasMine x) == False = (0 + (countMines xs))

-- Get cell based on i j coordinates from the backend board.
-- If index out of bounds returns a cell with cID=-1 and hasMine = False
getCell :: Int -> Int -> [[Cell]] -> Int -> Cell
getCell i j xs lim | ((i > ((length xs) - 1)) || (j > (lim - 1))) = (CellInstance False False (-1))
getCell i j xs lim | ((i < 0) || (j < 0)) = (CellInstance False False (-1))
getCell i j xs lim = do
                let row = (xs !! ((lim - 1) - i))
                (row !! (((length row) - 1) - j))
