-- Adapted from the Yampa package.
-- Displays a square moving in a circle. To move the position drag the mouse.
--
-- Requires the SDL package, assuming asyncly has already been built, you can
-- compile it like this:
-- stack ghc --package SDL circle-mouse.hs

import Data.IORef
import Graphics.UI.SDL as SDL
import Asyncly
import Asyncly.Time

------------------------------------------------------------------------------
-- SDL Graphics Init
------------------------------------------------------------------------------

sdlInit :: IO ()
sdlInit = do
  SDL.init [InitVideo]

  let width  = 640
      height = 480
  screen <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

------------------------------------------------------------------------------
-- Display a box at a given coordinates
------------------------------------------------------------------------------

display :: (Double, Double) -> IO ()
display (playerX, playerY) = do
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 55 60 64
  fillRect screen Nothing bgColor

  -- Paint small red square, at an angle 'angle' with respect to the center
  foreC <- mapRGB format 212 108 73
  let side = 10
      x = round playerX
      y = round playerY
  fillRect screen (Just (Rect x y side side)) foreC

  -- Double buffering
  SDL.flip screen

------------------------------------------------------------------------------
-- Wait and update Controller Position if it changes
------------------------------------------------------------------------------

refreshRate = 40

updateController :: IORef (Double, Double) -> IO ()
updateController ref = periodic refreshRate $ do
  e <- pollEvent
  case e of
    MouseMotion x y _ _ -> do
        writeIORef ref (fromIntegral x, fromIntegral y)
    _ -> return ()

------------------------------------------------------------------------------
-- Periodically refresh the output display
------------------------------------------------------------------------------

updateDisplay :: IORef (Double, Double) -> IO ()
updateDisplay cref = withClock clock refreshRate displaySquare

    where

    clock = do
        t <- SDL.getTicks
        return ((fromIntegral t) * 1000)

    speed  = 8
    radius = 30
    displaySquare time = do
        (x, y) <- readIORef cref
        let t = (fromIntegral time) * speed / 1000000
         in display (x + cos t * radius, y + sin t * radius)

main = do
  sdlInit
  cref <- newIORef (0,0)
  runAsyncly $  liftIO (updateController cref) <|> liftIO (updateDisplay cref)
