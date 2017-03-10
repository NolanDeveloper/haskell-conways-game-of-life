import Fxf
import Control.Monad.State
import Graphics.X11.Types
import qualified Data.Vector as V
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad

type Universe = V.Vector (V.Vector Bool)

data GameOfLife = MkGameOfLife Universe UTCTime

emptyUniverse :: Universe
emptyUniverse = V.replicate 40 $ V.replicate 40 False

makeLive :: Universe -> Int -> Int -> Universe
makeLive universe row col =
    let oldRow = universe V.! row
        newRow = oldRow V.// [(col, True)]
    in universe V.// [(row, newRow)]

isHabitable :: Universe -> Int -> Int -> Bool
isHabitable universe row col = universe V.! (row `mod` 40) V.! (col `mod` 40)

neighbours :: Int -> Int -> [(Int, Int)]
neighbours row col = do
    i <- [-1..1]
    j <- [-1..1]
    if i == 0 && j == 0
        then []
        else [(row - i, col - j)]

countNeighbours :: Universe -> Int -> Int -> Int
countNeighbours universe row col =
    let checkIfHabitable = \(r, c) -> isHabitable universe r c
        aliveNeighbours = filter checkIfHabitable $ neighbours row col
    in length aliveNeighbours

type IsHabitable = Bool
type NumOfNeighbours = Int

ruleOfGame :: IsHabitable -> NumOfNeighbours -> Bool
ruleOfGame True  2 = True
ruleOfGame True  3 = True
ruleOfGame False 3 = True
ruleOfGame _     _ = False

nextUniverse :: Universe -> Universe
nextUniverse universe =
    V.generate 40 $ \row ->
        V.generate 40 $ \col ->
            let neighbours = countNeighbours universe row col
                habitable = isHabitable universe row col
            in ruleOfGame habitable neighbours

calculateInitialState :: IO GameOfLife
calculateInitialState = do
    let content = [ ( 0,  1), ( 1,  2), ( 2,  0), ( 2,  1)
                  , ( 2,  2), ( 2, 11), ( 2, 12), ( 2, 13)
                  , ( 1, 12), ( 3, 12), ( 3, 13), ( 3, 14) ]
    let universe = go content emptyUniverse
            where
            go [] universe = universe
            go ((row, col):others) universe =
                let modifiedUniverse = makeLive universe row col
                in go others modifiedUniverse
    currentTime <- getCurrentTime
    return $ MkGameOfLife universe currentTime

drawUniverse :: Universe -> ContextAction GameOfLife ()
drawUniverse universe = do
    forM_ [0..39] $ \row ->
        forM_ [0..39] $ \col ->
            let habitable = isHabitable universe row col
            in setPixel row col habitable

simulationStep = 0.05

instance Game GameOfLife where
    keyDown key = return ()
    keyUp key = return ()
    update = do
        (MkGameOfLife universe lastUpdateTime) <- getGame
        currentTime <- lift getCurrentTime
        let dt = diffUTCTime currentTime lastUpdateTime
        if dt < simulationStep
            then return ()
            else do
                drawUniverse universe
                let newUniverse = nextUniverse universe
                setGame (MkGameOfLife newUniverse currentTime)

main = do
    initialState <- calculateInitialState
    runGame "Conway's life" initialState

