module Controller where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM.TChan
import System.IO
import Safe
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.SDL as Sdl
import qualified Graphics.UI.SDL.Image as Sdl
import qualified Graphics.UI.SDL.Time as Sdl
import View
import Util

type ViewState = StateT View IO
type ViewEnv   = ReaderT ViewConfig ViewState

type ControllerData  = Maybe (Int, Int)
type ControllerChan  = Chan (Bool, ControllerData)

view :: ControllerChan -> IO ()
view c = do
  ac <- initViewConfig
  evalStateT (runReaderT (loop c) ac) initView
  Sdl.quit

loop :: ControllerChan -> ViewEnv ()
loop c = do
  t  <- startTicks
  (q, pos) <- liftIO $ readChan c
  update (ViewInput pos)
  output
  delayView t
  unless q (loop c)
 
update :: ViewInput -> ViewEnv ()
update ai = modify $ updateView ai

output :: ViewEnv ()
output = ask >>= \ac -> get >>= \app -> liftIO $ outputView ac app

--

controller :: ControllerChan -> IO ()
controller c = do
  Gtk.initGUI
  initController c
  Gtk.mainGUI

--

initController :: ControllerChan -> IO ()
initController c = do
  wn <- Gtk.windowNew
  hb <- Gtk.hBoxNew True 2
  ex <- Gtk.entryNew
  ey <- Gtk.entryNew
  Gtk.entrySetText ex "0"
  Gtk.entrySetText ey "0"
  Gtk.set wn [ Gtk.windowDefaultWidth   Gtk.:= 50
             , Gtk.windowDefaultHeight  Gtk.:= 40
             , Gtk.containerBorderWidth Gtk.:= 10
             , Gtk.containerChild       Gtk.:= hb ]
  Gtk.boxPackStart hb ex Gtk.PackNatural 0
  Gtk.boxPackStart hb ey Gtk.PackNatural 0
  Gtk.onDestroy wn Gtk.mainQuit
  Gtk.widgetShowAll wn
  Gtk.windowSetTitle wn "Controller"
  Gtk.timeoutAdd (sendPos ex ey c >> return True) 100
  Gtk.quitAdd 0 (writeChan c (True, Nothing) >> return False)
  return ()

--

sendPos :: Gtk.Entry -> Gtk.Entry -> ControllerChan -> IO ()
sendPos ex ey c = do
  mx <- getEntryInt ex
  my <- getEntryInt ey
  writeChan c $ (False, joinTuple (mx, my))

joinTuple (mx, my) = mx >>= \x -> my >>= \y -> Just (x,y)

getEntryInt :: Gtk.Entry -> IO (Maybe Int)
getEntryInt e = Gtk.entryGetText e >>= \t -> return $ readMay t

