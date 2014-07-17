module View where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import System.IO
import qualified Graphics.UI.SDL as Sdl
import qualified Graphics.UI.SDL.Image as Sdl
import qualified Graphics.UI.SDL.Time as Sdl
import Util


data View       = View { myPosition :: (Int,Int) }
data ViewConfig = ViewConfig { screen :: Sdl.Surface, myImage :: Sdl.Surface }
data ViewInput  = ViewInput { myGUIPosition :: Maybe (Int, Int) }


initViewConfig :: IO ViewConfig
initViewConfig = do
  s <- Sdl.setVideoMode 320 240 32 [Sdl.SWSurface]
  Sdl.setCaption "View" []
  img <- loadImage "stickfigure.bmp" $ Just magenta
  return $ ViewConfig s img


initView :: View
initView = View (0,0)


updateView :: ViewInput -> View -> View
updateView (ViewInput mXY) app = case mXY of (Just xy) -> View xy
                                             Nothing   -> app


white   = (255,255,255)
black   = (  0,  0,  0)
red     = (255,  0,  0)
green   = (  0,255,  0)
blue    = (  0,  0,255)
yellow  = (255,255,  0) 
magenta = (255,  0,255)
cyan    = (  0,255,255)


outputView :: ViewConfig -> View -> IO ()
outputView ac app = do
  clearSurface black $ screen ac
  drawSurface (myPosition app) (myImage ac) (screen ac)
  Sdl.flip $ screen ac

framesPerSec = 60

delayView :: MonadIO m => Word32 -> m ()
delayView ms = delay (milisecPerSec `div` framesPerSec)  ms


