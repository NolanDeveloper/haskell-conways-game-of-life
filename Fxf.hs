module Fxf(
    ContextAction,
    Game(..),
    clearScreen,
    setPixel,
    getPixel,
    runGame
) where

import Graphics.X11.Xlib.Window
import qualified Graphics.X11.Xlib.Types as X
import Graphics.X11.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Context
import Graphics.X11.Xlib.Misc
import Data.Time.Clock
import Data.Bits
import qualified Data.Vector as V
import Control.Monad.Trans.State
import Control.Monad

type Screen = V.Vector (V.Vector Bool)

screenSize = 40

emptyScreen :: Screen
emptyScreen = V.replicate screenSize $ V.replicate screenSize False

keyCode :: XKeyEvent -> KeyCode
keyCode (_, _, _, _, _, _, _, _, code, _) = code

data Context
    = MkContext
    { contextDisplay :: X.Display
    , contextWindow :: Drawable
    , contextGC :: X.GC
    , contextBlack :: X.Pixel
    , contextWhite :: X.Pixel
    , contextEventPtr :: XEventPtr
    , contextScreen :: Screen
    }

type ContextAction = StateT Context IO

class Game a where
    gameKeyDown :: a -> KeySym -> ContextAction a
    gameKeyUp :: a -> KeySym -> ContextAction a
    gameUpdate :: a -> ContextAction a

processExposeEvent :: Context -> IO ()
processExposeEvent context@(MkContext
        display window gc black white eventPtr screen) = do
    setForeground display gc black
    fillRectangle display window gc 0 0 400 400
    setForeground display gc white
    forM_ [0..39] $ \row ->
        forM_ [0..39] $ \col ->
            if screen V.! row V.! col
                then let x = fromIntegral $ col * 10
                         y = fromIntegral $ row * 10
                     in fillRectangle display window gc x y 10 10
                else return ()
    sendEvent display window False exposureMask eventPtr

processKeyEvent :: Game a => Bool -> Context -> a -> IO (a, Context)
processKeyEvent isKeyDown context game = do
    let eventPtr = contextEventPtr context
    let display = contextDisplay context
    keyEvent <- get_KeyEvent eventPtr
    let code = keyCode keyEvent
    let handler = if isKeyDown then gameKeyDown else gameKeyUp
    keySym <- keycodeToKeysym display code 0
    (game', context') <- runStateT (handler game keySym) context
    return (game', context')

processEvent :: Game a => Context -> a -> IO (a, Context)
processEvent context game = do
    let display = contextDisplay context
    let eventPtr = contextEventPtr context
    nextEvent display eventPtr
    eventType <- get_EventType eventPtr
    if eventType == expose
        then do
            processExposeEvent context
            return (game, context)
        else if eventType `elem` [keyPress, keyRelease]
            then processKeyEvent (eventType == keyPress) context game
            else return (game, context)

loop :: Game a => Context -> a -> IO ()
loop context game = do
    nPending <- pending $ contextDisplay context
    if 0 /= nPending
        then do
            (game', context') <- processEvent context game
            loop context' game'
        else do
            (game', context') <- runStateT (gameUpdate game) context
            loop context' game'

clearScreen :: ContextAction ()
clearScreen = do
    context <- get
    put $ context { contextScreen = emptyScreen }

setPixel :: Int -> Int -> Bool -> ContextAction ()
setPixel row col value = do
    context <- get
    let screen = contextScreen context
    let oldRow = screen V.! row
    let newRow = oldRow V.// [(col, value)]
    put $ context { contextScreen = screen V.// [(row, newRow)] }

getPixel :: Int -> Int -> ContextAction Bool
getPixel row col = do
    context <- get
    let screen = contextScreen context
    return $ screen V.! row V.! col

runGame :: Game a => String -> a -> IO ()
runGame name initialState = do
    display <- openDisplay ""
    let screen = defaultScreen display
    let black = blackPixel display screen
    let white = whitePixel display screen
    parent <- rootWindow display screen
    window <- createSimpleWindow display parent 0 0 400 400 0 black white
    storeName display window name
    gc <- createGC display window
    let mask = exposureMask .|. keyPressMask .|. keyReleaseMask
    selectInput display window mask
    mapWindow display window
    allocaXEvent $ \ptr ->
        let context = MkContext display window gc black white ptr emptyScreen
        in loop context initialState
    freeGC display gc
    destroyWindow display window
    closeDisplay display

