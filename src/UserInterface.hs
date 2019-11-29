module UserInterface
    (
      setup
    ) where

{-# LANGUAGE ForeignFunctionInterface #-}


import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as Core
import System.CPUTime                (getCPUTime)
import Minegrid
import MinegridGame

-- Initial set up of game. Called when first started and when reset button is
-- is clicked (which is only visible when game lost)
setup :: Window -> UI ()
setup window = do
        blehh <- liftIO $ getCPUTime
        let board = setupBoard 15 15 (fromIntegral blehh) 0.2
        grid <- (getElementsByClassName window ("board_active"))
        msg <- (getElementsByClassName window ("lost_message"))
        btn <- (getElementsByClassName window ("reset_btn"))
        aibtn <- (getElementsByClassName window ("aiMove"))
        if (length grid > 0 && length msg > 0 && length btn > 0 && length aibtn > 0)
          then do
            deleteGrid (head grid)
            deleteGrid (head msg)
            deleteGrid (head btn)
            deleteGrid (head aibtn)
          else return ()
        return window # set title "Minesweepin Paddys"
        let originalRows = createGrid board window "board_nonactive"
        getBody window #+ originalRows
        return ()

-- Updates the state of the GUI board when a new move is made
runGame :: [[Cell]] -> Window -> UI ()
runGame newBoard window = do
          nonactive <- (getElementsByClassName window ("board_nonactive"))
          if (length nonactive > 0)
              then do
                  deleteGrid (head nonactive)
              else return ()
          active <- (getElementsByClassName window ("board_active"))
          if (length active > 0)
              then do
                  deleteGrid (head active)
              else return ()
          aibutton <- (getElementsByClassName window ("aiMove"))
          if (length aibutton > 0)
              then do
                  deleteGrid (head aibutton)
              else return ()
          return window # set title "Minesweepin Paddys"
          let rows = createGrid newBoard window "board_active"
          getBody window #+ rows
          makeai <- UI.button # set text "make ai move"
                              # set (attr "class") ("aiMove")
          getBody window #+ [element makeai]
          on UI.click makeai $ \_ -> do
                makeAiMove newBoard window
          return ()

-- Creates a button to be used in the browser minesweeper game board
makeButton :: Cell -> [[Cell]] -> Window -> UI Element
makeButton cell board w = do
        activeGameList <- getElementsByClassName w "board_active"
        -- Assign variables concerned with relevant cell
        let matchingCell = cell
        let mine = hasMine matchingCell
        let cellId = cId matchingCell
        let clicked = isClicked matchingCell
        let activeGame = do
                    if (length activeGameList >= 0 && clicked == True)
                      then True
                      else False
        let prox = proximity matchingCell
        let proxString = do
                    if (clicked == True)
                      then (show prox)
                      else "?"
        let color = do
                    if (proxString /= "?")
                      then "#00FFFF"
                      else "A9A9A9"

        -- Define baseline cell attributes
        button <- UI.button # set text proxString
                            # set value proxString
                            # set (attr "id") (show cellId)
                            # set (attr "oncontextmenu") ("return false;")
                            # set (attr "class") ("cell")
                            # set (attr "style") (("background-color:" ++ color ++ ";"))

        on UI.click button $ \_ -> do
                active <- getElementsByClassName w "board_active"
                if (length active <= 0)
                  then do
                      let newBoard = forceClearById board matchingCell
                      runGame newBoard w
                      runFunction $ generateClick cellId
                      liftIO $ print "New Game Started!!"
                    -- Get the neighbours, if any of them are marked as mines,
                    -- Override the larry.
                  else do
                      case mine of
                        True -> do
                          element button # set text "M"
                                         # set (attr "value") "M"
                                         # set (attr "style") (("background-color:#DC143C;"))
                          getBody w #+ [UI.h1 #+ [string "YOU LOST"] # set (attr "class") ("lost_message")]
                          reset <- UI.button # set text "Reset?" # set (attr "class") ("reset_btn")
                          getBody w #+ [element reset]
                          on UI.click reset $ \_ -> do
                                     setup w
                        False -> do
                          let new = update board matchingCell
                          let hasWon = checkWin new
                          let str = do
                                if (hasWon)
                                  then "Mup!! You won!!"
                                  else ""
                          el <- UI.h1 #+ [string str] # set (attr "class") ("win_message")
                          getBody w #+ [element el]
                          element button # set text proxString
                                         # set (attr "style") ("background-color:#00FFFF;")
                          runGame new w

        on UI.contextmenu button $ \_ -> do
                element button # set text "F"
        on UI.hover button $ \_ -> do
                element button # set style [("color", "#DC143C")]
        on UI.leave button $ \_ -> do
                element button # set style [("color", "#000000")]

        return button

-- Very basic and unreliable ai (ran out of time!!!) move that executes when make ai move button
-- is clicked. Ranks unrevealed border cells from riskiest to safest and picks
-- a random cell from the best available option
makeAiMove :: [[Cell]] -> Window -> UI ()
makeAiMove board window = do
        let revealed = filter (\x -> (isClicked x) == True && (hasMine x) == False) (concat board)
        let unsafeRevealed = filter (\x -> (proximity x) >= 6) revealed
        let avgRevealed = filter (\x -> ((proximity x) < 6 && (proximity x) >= 4)) revealed
        let safeRevealed = filter (\x -> (proximity x) < 4) revealed

        let safeToClick = filter (\x -> isClicked x == False) (concat (getClickables safeRevealed board))
        let avgToClick = filter (\x -> isClicked x == False) (concat (getClickables avgRevealed board))
        let unsafeToClick = filter (\x -> isClicked x == False) (concat (getClickables unsafeRevealed board))

        rand <- liftIO $ getCPUTime
        let index1 = mod (fromIntegral rand) (length safeToClick)
        let index2 = mod (fromIntegral rand) (length avgToClick)
        let index3 = mod (fromIntegral rand) (length unsafeToClick)

        if (length safeRevealed > 0)
          then runFunction $ generateClick (cId (safeToClick !! index1))
          else if (length avgRevealed > 0)
            then runFunction $ generateClick (cId (avgToClick !! index2))
            else runFunction $ generateClick (cId (unsafeToClick !! index3))
        return ()

-- Returns neighbouring cells of given clicked elements
getClickables xs@[] _ = []
getClickables (x:cells) board = do
        ((getNeigbourooneys (pos x) board):(getClickables cells board))

-- Generates a click event that targets a button based on its id (Very handy!)
generateClick :: Int -> JSFunction ()
generateClick = ffi "var cell = document.getElementById(%1); if (cell.value === '?') {cell.dispatchEvent(new Event('click'));}"

-- Clears redundant elements
deleteGrid grid = do
        delete grid

-- Creates a grid for setup and runGame functions
createGrid b w className = do
  rows <- [grid (generateGrid b [[]] b w) # set (attr "class") (className)]
  return rows

-- generateGrid and generateRow create a frontend representation of [[Cell]]
generateGrid [] ys b w = ys
generateGrid (x:xs) ys b w = generateGrid xs ((generateRow x [] b w):ys) b w
-- genrateRow
generateRow [] ys b w = ys
generateRow (x:xs) ys b w = generateRow xs ((makeButton x b w):ys) b w
