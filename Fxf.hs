module Fxf(
    ContextAction,
    Game(..),
    clearScreen,
    setPixel,
    getPixel,
    setGame,
    getGame,
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
import Glyphs

type Screen = V.Vector (V.Vector Bool)

screenSize = 40

emptyScreen :: Screen
emptyScreen = V.replicate screenSize $ V.replicate screenSize False

keyCode :: XKeyEvent -> KeyCode
keyCode (_, _, _, _, _, _, _, _, code, _) = code

data Context a
    = MkContext
    { contextDisplay :: X.Display
    , contextWindow :: Drawable
    , contextGC :: X.GC
    , contextBlack :: X.Pixel
    , contextWhite :: X.Pixel
    , contextEventPtr :: XEventPtr
    , contextScreen :: Screen
    , contextGame :: a
    }

type ContextAction a = StateT (Context a) IO

class Game a where
    keyDown :: KeySym -> ContextAction a ()
    keyUp :: KeySym -> ContextAction a ()
    update :: ContextAction a ()

processExposeEvent :: Game a => Context a -> IO (Context a)
processExposeEvent context@(MkContext
        display window gc black white eventPtr screen _) = do
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
    return context

processKeyEvent :: Game a => Bool -> Context a -> IO (Context a)
processKeyEvent isKeyDown context = do
    let eventPtr = contextEventPtr context
    let display = contextDisplay context
    keyEvent <- get_KeyEvent eventPtr
    let code = keyCode keyEvent
    let handler = if isKeyDown then keyDown else keyUp
    keySym <- keycodeToKeysym display code 0
    ((), context') <- runStateT (handler keySym) context
    return context'

processEvent :: Game a => Context a -> IO (Context a)
processEvent context = do
    let display = contextDisplay context
    let eventPtr = contextEventPtr context
    nextEvent display eventPtr
    eventType <- get_EventType eventPtr
    if eventType == expose
        then processExposeEvent context
        else if eventType `elem` [keyPress, keyRelease]
            then processKeyEvent (eventType == keyPress) context
            else return context

loop :: Game a => Context a -> IO ()
loop context = do
    nPending <- pending $ contextDisplay context
    if 0 /= nPending
    then do
        context' <- processEvent context
        loop context'
    else do
        ((), context') <- runStateT update context
        loop context'

clearScreen :: ContextAction a ()
clearScreen = do
    context <- get
    put $ context { contextScreen = emptyScreen }

setPixel :: Int -> Int -> Bool -> ContextAction a ()
setPixel row col value = do
    context <- get
    let screen = contextScreen context
    let oldRow = screen V.! row
    let newRow = oldRow V.// [(col, value)]
    put $ context { contextScreen = screen V.// [(row, newRow)] }

getPixel :: Int -> Int -> ContextAction a Bool
getPixel row col = do
    context <- get
    let screen = contextScreen context
    return $ screen V.! row V.! col

getGame :: ContextAction a a
getGame = do
    context <- get
    return $ contextGame context

setGame :: a -> ContextAction a ()
setGame game = do
    context <- get
    put $ context { contextGame = game }

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
        loop $ MkContext display window gc black white ptr
                         emptyScreen initialState
    freeGC display gc
    destroyWindow display window
    closeDisplay display

