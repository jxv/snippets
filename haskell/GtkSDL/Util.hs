module Util where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Graphics.UI.SDL as Sdl
import qualified Graphics.UI.SDL.Image as Sdl
import qualified Graphics.UI.SDL.Time as Sdl


loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Sdl.Surface
loadImage s ck = Sdl.load s >>= Sdl.displayFormat >>= setColorKey' ck
  where
    setColorKey' Nothing s = return s
    setColorKey' (Just(r,g,b)) s  = do
      p <- (Sdl.mapRGB . Sdl.surfaceGetPixelFormat) s r g b
      Sdl.setColorKey s [Sdl.SrcColorKey] p
      return s


fromIntegral' x y = (fromIntegral x, fromIntegral y)


drawSurface :: (Int, Int) -> Sdl.Surface -> Sdl.Surface -> IO Bool
drawSurface (x,y) s s' = Sdl.blitSurface s Nothing s' offset
  where offset = Just $ Sdl.Rect x y 0 0


drawRect :: Sdl.Rect -> (Word8, Word8, Word8) -> Sdl.Surface -> IO Bool
drawRect rect (r,g,b) s = do
  p <- (Sdl.mapRGB . Sdl.surfaceGetPixelFormat) s r g b
  Sdl.fillRect s (Just rect) p


clearSurface :: (Word8, Word8, Word8) -> Sdl.Surface -> IO Bool
clearSurface rgb s = do
  cr <- Sdl.getClipRect s
  drawRect cr rgb s


milisecPerSec :: Word32
milisecPerSec = 1000


startTicks :: MonadIO m => m Word32
startTicks = liftIO Sdl.getTicks


delay :: MonadIO m => Word32 -> Word32 -> m ()
delay spf ms = liftIO $ do
  t <- Sdl.getTicks
  let t' = ms - t
  when (t' < spf) $ Sdl.delay $ spf - t'


