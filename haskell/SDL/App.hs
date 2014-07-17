module App where

import Data.Word
import Control.Monad.State
import qualified Graphics.UI.SDL as Sdl
import qualified Graphics.UI.SDL.Image as Sdl ()
import qualified Graphics.UI.SDL.Time as Sdl ()
import Util


data App       = App { myPosition :: (Int,Int) }
data AppConfig = AppConfig { screen :: Sdl.Surface, myImage :: Sdl.Surface }
data AppInput  = AppInput { myMouse :: Maybe (Int, Int) }


initAppConfig :: IO AppConfig
initAppConfig = do
  s <- Sdl.setVideoMode 320 240 32 [Sdl.SWSurface]
  Sdl.setCaption "Template" []
  img <- loadImage "stickfigure.bmp" $ Just magenta
  return $ AppConfig s img


initApp :: App
initApp = App (0,0)


mkAppInput :: AppInput
mkAppInput = AppInput Nothing


inputApp :: AppInput -> Sdl.Event -> AppInput
inputApp ai e = case e of (Sdl.MouseButtonDown x y Sdl.ButtonLeft) -> AppInput $ Just $ fromIntegral' x y
                          _ -> ai


updateApp :: AppInput -> App -> App
updateApp (AppInput mXY) app = case mXY of (Just xy) -> App xy
                                           Nothing   -> app


quitApp :: App -> Bool
quitApp _ = False


white :: Num a => (a, a, a)
white   = (255,255,255)

black :: Num a => (a, a, a)
black   = (  0,  0,  0)

red :: Num a => (a, a, a)
red     = (255,  0,  0)

green :: Num a => (a, a, a)
green   = (  0,255,  0)

blue :: Num a => (a, a, a)
blue    = (  0,  0,255)

yellow :: Num a => (a, a, a)
yellow  = (255,255,  0) 

magenta :: Num a => (a, a, a)
magenta = (255,  0,255)

cyan :: Num a => (a, a, a)
cyan    = (  0,255,255)


outputApp :: AppConfig -> App -> IO ()
outputApp ac app = do
  void $ clearSurface black (screen ac)
  void $ drawSurface (myPosition app) (myImage ac) (screen ac)
  Sdl.flip $ screen ac


-- Frames Per Second
fps :: Num a => a
fps = 60

delayApp :: MonadIO m => Word32 -> m ()
delayApp ms = delay (msps `div` fps)  ms


