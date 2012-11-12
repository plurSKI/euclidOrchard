{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Word
import Data.List
import System.Exit

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image as SDLImage

import Timer
import Euclid
import HandleFiles

import qualified Graphics.UI.SDL.Mixer.General as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Music as SDL.Mixer.Music
import qualified Graphics.UI.SDL.Mixer.Types as SDL.Mixer.Types

(screenWidth, screenHeight, screenBpp)  = (640, 480, 32)
(dotWidth, dotHeight)                   = (20, 20)
(tileWidth, tileHeight)                 = (80, 80)
(totalTiles, tileSprites)               = (192, 12)
(levelWidth, levelHeight)               = (100*tileWidth, 100*tileHeight)

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = SDLImage.load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m Bool
applySurface' x y src dst = liftIO . applySurface x y src dst

intersects :: Rect -> Rect -> Bool
intersects (Rect ax ay aw ah) (Rect bx by bw bh) =
    bottomA > topB && topA < bottomB && rightA > leftB && leftA < rightB
 where
    leftA   = ax
    rightA  = ax + aw
    topA    = ay
    bottomA = ay + ah
    
    leftB   = bx
    rightB  = bx + bw
    topB    = by
    bottomB = by + bh

data TileType =
      Floor
    | Exit
    | Blue
    | Center
    | Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | TopLeft
 deriving (Eq, Ord, Enum, Bounded, Show)

data Dot = Dot { dBox :: Rect, vel :: (Int, Int) }

data Tile = Tile { tBox     :: Rect
                 , tileType :: TileType
                 }

tile :: Int -> Int -> TileType -> Tile
tile x y t = Tile { tBox=Rect x y tileWidth tileHeight, tileType=t }

touchesExit :: Rect -> [Tile] -> Bool
touchesExit b = any touches
 where touches (Tile tB tType) =
          if tType == Exit
            then intersects b tB
            else False

touchesWall :: Rect -> [Tile] -> Bool
touchesWall b = any touches
 where touches (Tile tB tType) =
        let eType = fromEnum tType
        in if eType >= fromEnum Center && eType <= fromEnum TopLeft
            then intersects b tB
            else False

handleInput :: Event -> Dot -> Dot
handleInput (KeyDown (Keysym SDLK_UP _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy - dotHeight) }
handleInput (KeyDown (Keysym SDLK_w _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy - dotHeight) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy + dotHeight) }
handleInput (KeyDown (Keysym SDLK_s _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy + dotHeight) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx - dotWidth, dy) }
handleInput (KeyDown (Keysym SDLK_a _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx - dotWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx + dotWidth, dy) }
handleInput (KeyDown (Keysym SDLK_d _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx + dotWidth, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy + dotHeight) }
handleInput (KeyUp (Keysym SDLK_w _ _)) d@Dot { vel=(dx,dy) }    = d { vel=(dx, dy + dotHeight) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy - dotHeight) }
handleInput (KeyUp (Keysym SDLK_s _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx, dy - dotHeight) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx + dotWidth, dy) }
handleInput (KeyUp (Keysym SDLK_a _ _)) d@Dot { vel=(dx,dy) }  = d { vel=(dx + dotWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx - dotWidth, dy) }
handleInput (KeyUp (Keysym SDLK_d _ _)) d@Dot { vel=(dx,dy) } = d { vel=(dx - dotWidth, dy) }

handleInput _ d = d

move :: [Tile] -> Dot -> Dot
move tiles d@Dot { dBox=b@(Rect x y _ _), vel=(dx,dy) } = d { dBox=b { rectX=checkXPos, rectY=checkYPos } }
 where
    (x', y') = (x + dx, y + dy)
    checkXPos | x' < 0 || (x' + dotWidth) > levelWidth || touchesWall b {rectX=x'} tiles = x 
              | otherwise = x'
    checkYPos | y' < 0 || (y' + dotHeight) > levelHeight || touchesWall b {rectX=checkXPos,rectY=y'} tiles = y 
              | otherwise = y'

type Camera = Rect

setCamera :: Dot -> Camera -> Camera
setCamera Dot { dBox=Rect x y _ _ } rect@(Rect _ _ w h) = rect { rectX=x'', rectY=y'' }
 where
    x'  = (x + dotWidth `div` 2) - screenWidth `div` 2
    y'  = (y + dotHeight `div` 2) - screenHeight `div` 2
    x'' = min (levelWidth - w) $ max x' 0
    y'' = min (levelHeight - h) $ max y' 0 

setTiles :: IO [Tile]
setTiles = do
    tileMap <- wordsToInt `liftM` readFile "data/euclid.map"
    evalStateT (mapM mkTile tileMap) (0,0)
 where wordsToInt :: String -> [Int]
       wordsToInt = map read . words
       mkTile :: MonadState (Int,Int) m => Int -> m Tile
       mkTile t = do
        (x, y) <- get
        if (x + tileWidth >= levelWidth)
            then put (0, y + tileHeight)
            else put (x + tileWidth, y)
        
        if t >= 0 && t < tileSprites
            then return $ tile x y $ toEnum t
            else error "failed to parse tile"

clips :: [Rect]
clips = [
    -- red
    Rect 0 0 tileWidth tileHeight,
    -- green
    Rect 0 80 tileWidth tileHeight,
    -- Blue
    Rect 0 160 tileWidth tileHeight,
    -- Center
    Rect 160 80 tileWidth tileHeight,
    -- Top
    Rect 160 0 tileWidth tileHeight,
    -- TopRight
    Rect 240 0 tileWidth tileHeight,
    -- Right
    Rect 240 80 tileWidth tileHeight,
    -- BottomRight
    Rect 240 160 tileWidth tileHeight,
    -- Bottom
    Rect 160 160 tileWidth tileHeight,
    -- BottomLeft
    Rect 80 160 tileWidth tileHeight,
    -- Left
    Rect 80 80 tileWidth tileHeight,
    -- TopLeft
    Rect 80 0 tileWidth tileHeight]

data AppData = AppData {
    adDot  :: Dot,
    camera :: Camera,
    fps    :: Timer
}

data AppConfig = AppConfig {
    screen     :: Surface,
    dotSprite  :: Surface,
    tileSheet  :: Surface,
    tiles      :: [Tile],
    musicFile  :: SDL.Mixer.Types.Music
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getDot :: MonadState AppData m => m Dot
getDot = liftM adDot get

putDot :: MonadState AppData m => Dot -> m ()
putDot t = modify $ \s -> s { adDot = t }

modifyDotM :: MonadState AppData m => (Dot -> m Dot) -> m ()
modifyDotM act = getDot >>= act >>= putDot

modifyDot :: MonadState AppData m => (Dot -> Dot) -> m ()
modifyDot fn = fn `liftM` getDot >>= putDot

getCamera :: MonadState AppData m => m Camera
getCamera = liftM camera get

putCamera :: MonadState AppData m => Camera -> m ()
putCamera t = modify $ \s -> s { camera = t }

modifyCameraM :: MonadState AppData m => (Camera -> m Camera) -> m ()
modifyCameraM act = getCamera >>= act >>= putCamera

modifyCamera :: MonadState AppData m => (Camera -> Camera) -> m ()
modifyCamera fn = fn `liftM` getCamera >>= putCamera

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getDotSprite :: MonadReader AppConfig m => m Surface
getDotSprite = liftM dotSprite ask

getTileSheet :: MonadReader AppConfig m => m Surface
getTileSheet = liftM tileSheet ask

getTiles :: MonadReader AppConfig m => m [Tile]
getTiles = liftM tiles ask

showTile :: (MonadIO m, MonadReader AppConfig m) => Camera -> Tile -> m ()
showTile camera@(Rect cx cy _ _) (Tile tbox@(Rect tx ty _ _) tileType) =
    when (intersects tbox camera) $ do
        screen    <- getScreen
        tileSheet <- getTileSheet
        applySurface' (tx - cx) (ty - cy) tileSheet screen $ Just $ clips !! tileIndex
        return ()
 where tileIndex = fromEnum tileType

showDot :: (MonadIO m, MonadReader AppConfig m) => Camera -> Dot -> m ()
showDot (Rect cx cy _ _) (Dot (Rect dx dy _ _) _) = do
    screen    <- getScreen
    dotSprite <- getDotSprite
    applySurface' (dx - cx) (dy - cy) dotSprite screen Nothing
    return () 

initEnv :: (String, String,String) -> IO (AppConfig, AppData)
initEnv (dPng, tPng, mMp3) = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface , Fullscreen]
    setCaption "--------------------------" []
    makeTmpDir
    mMp3' <- getFile mMp3
    mus   <- SDL.Mixer.Music.loadMUS mMp3'
    SDL.Mixer.Music.setMusicVolume 100
    SDL.Mixer.Music.playMusic mus (-1)
    dPng' <- getFile dPng
    tPng' <- getFile tPng
    dotSprite <- loadImage dPng' $ Just (0x00, 0xff, 0xff)
    tileSheet <- loadImage tPng' Nothing
    rmTmpDir
    tiles     <- setTiles
    fps       <- start defaultTimer

    return (AppConfig screen dotSprite tileSheet tiles mus, AppData myDot camera fps) 
 where
    myDot  = Dot (Rect 0 0 dotWidth dotHeight) (0,0)
    camera = Rect 0 0 screenWidth screenHeight

loop :: AppEnv ()
loop = do
    
    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyDot . handleInput
    
    tiles <- getTiles
    modifyDot $ move tiles
    
    myDot <- getDot
    modifyCamera $ setCamera myDot
    
    camera <- getCamera
    forM_ tiles $ showTile camera 
       
    showDot camera myDot
   
    screen <- getScreen
    fps    <- getFPS
    liftIO $ do                
        Graphics.UI.SDL.flip screen
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks
    if quit 
      then liftIO $ exitSuccess
      else unless (touchesExit (dBox myDot) tiles) loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        (KeyDown (Keysym SDLK_ESCAPE _ _)) -> return True
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

newRun 3 = do makeTmpDir
              getFile "ending.mpeg"
              putStrLn "Good End."
              exitWith (ExitFailure 666)

newRun x = withInit [InitEverything] $ do
    boardImage [(x,y) | x <- [1..dim], y <- [1..dim]] 5 "data/euclid"
    SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16Sys 2 4096
    let c = show x
    (env, state) <- initEnv ( "dot" ++ c ++ ".png"
                            , "tiles" ++ c ++ ".png"
                            , "music" ++ c ++ ".mp3" 
                            )
    runLoop env state
    quitSubSystem [InitEverything]
    SDL.Mixer.closeAudio
    newRun (x + 1)

main = newRun 0
