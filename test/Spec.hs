import Test.Hspec.Expectations
import Test.Hspec.Core.Formatters (writeLine)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import Minegrid

main :: IO ()
main = do
    cpuT <- getCPUTime
    let gameSeed = fromIntegral cpuT
    runHspecUnitTests gameSeed

runHspecUnitTests seed = hspec $ do
    describe "Test setupBoard creates properly formed minesweeper grid" $ do
        it "setupBoard 0 0 0 creates a 0x0 grid with no mines" $ do
            setupBoard 0 0 seed 0 `shouldMatchList` [[]]
            setupBoard 0 0 seed 0.5 `shouldMatchList` [[]]
            setupBoard 0 0 seed 1 `shouldMatchList` [[]]


        it "setupBoard 2 2 0 creates a 2x2 grid with no mines" $ do
            setupBoard 2 2 seed 0 `shouldMatchList`
                    [[CellInstance {hasMine = False, isClicked = False},
                      CellInstance {hasMine = False, isClicked = False}],
                     [CellInstance {hasMine = False, isClicked = False},
                      CellInstance {hasMine = False, isClicked = False}]
                    ]

        it "setupBoard 2 2 1 creates a 2x2 grid with mines in every cell" $ do
            setupBoard 2 2 seed 1 `shouldMatchList`
                    [[CellInstance {hasMine = True, isClicked = False},
                      CellInstance {hasMine = True, isClicked = False}],
                     [CellInstance {hasMine = True, isClicked = False},
                      CellInstance {hasMine = True, isClicked = False}]
                    ]
