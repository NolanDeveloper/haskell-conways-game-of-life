import Fxf
import Control.Monad.State
import Graphics.X11.Types
import qualified Data.Vector as V
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad

type Universe = V.Vector (V.Vector Bool)

data GameOfLife = MkPreparation Universe Int Int
                | MkSimulation Universe UTCTime

emptyUniverse :: Universe
emptyUniverse = V.replicate 40 $ V.replicate 40 False

isHabitable :: Universe -> Int -> Int -> Bool
isHabitable universe row col = universe V.! (row `mod` 40) V.! (col `mod` 40)

setCell :: Universe -> Int -> Int -> Bool -> Universe
setCell universe row col alive =
    let oldRow = universe V.! row
        newRow = oldRow V.// [(col, alive)]
    in universe V.// [(row, newRow)]

revive :: Universe -> Int -> Int -> Universe
revive universe row col = setCell universe row col True

kill :: Universe -> Int -> Int -> Universe
kill universe row col = setCell universe row col False

switchCell :: Universe -> Int -> Int -> Universe
switchCell universe row col =
    let habitable = isHabitable universe row col
    in setCell universe row col (not habitable)

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

drawUniverse :: Universe -> ContextAction ()
drawUniverse universe = do
    forM_ [0..39] $ \row ->
        forM_ [0..39] $ \col ->
            let habitable = isHabitable universe row col
            in setPixel row col habitable

simulationStep = 0.05

switchPixel :: Int -> Int -> ContextAction ()
switchPixel row col = do
    oldPixel <- getPixel row col
    let newPixel = not oldPixel
    setPixel row col newPixel

drawMainDiagonal :: Int -> Int -> ContextAction ()
drawMainDiagonal row col = do
    if row < col
        then go 0 (col - row)
        else go (row - col) 0
    where
    go 40 _ = return ()
    go _ 40 = return ()
    go row col = do
        switchPixel row col
        go (row + 1) (col + 1)

drawSubDiagonal :: Int -> Int -> ContextAction ()
drawSubDiagonal row col = do
    if col + row < 40
        then go 0 (col + row)
        else go (row - (39 - col)) 39
    where
    go 40 _ = return ()
    go _ (-1) = return ()
    go row col = do
        switchPixel row col
        go (row + 1) (col - 1)

drawCursor :: Int -> Int -> ContextAction ()
drawCursor row col = do
    drawSubDiagonal row col
    drawMainDiagonal row col

instance Game GameOfLife where
    gameKeyDown game@(MkPreparation universe row col) key =
        let decMod x = (x - 1) `mod` 40
            incMod x = (x + 1) `mod` 40
            returnPreparation row col = return $ MkPreparation universe row col
        in case () of
        () | xK_Left  == key -> returnPreparation row (decMod col)
           | xK_Right == key -> returnPreparation row (incMod col)
           | xK_Up    == key -> returnPreparation (decMod row) col
           | xK_Down  == key -> returnPreparation (incMod row) col
           | xK_space == key ->
               let universe' = switchCell universe row col
               in return $ MkPreparation universe' row col
           | xK_Return == key -> do
               currentTime <- lift getCurrentTime
               return $ MkSimulation universe currentTime
           | otherwise       -> return game
    gameKeyDown game@(MkSimulation universe _) key
        | xK_Return == key = return $ MkPreparation universe 20 20
        | otherwise        = return game

    gameKeyUp game key = return game

    gameUpdate game@(MkPreparation universe row col) = do
        drawUniverse universe
        drawCursor row col
        return game

    gameUpdate game@(MkSimulation universe lastUpdateTime) = do
        currentTime <- lift getCurrentTime
        let dt = diffUTCTime currentTime lastUpdateTime
        if dt < simulationStep
            then return game
            else do
                drawUniverse universe
                let newUniverse = nextUniverse universe
                return (MkSimulation newUniverse currentTime)

main = do
    let game = MkPreparation emptyUniverse 20 20
    runGame "Conway's life" game

