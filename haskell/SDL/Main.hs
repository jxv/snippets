module Main where

import Data.Word ()
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Graphics.UI.SDL as Sdl
import qualified Graphics.UI.SDL.Image as Sdl ()
import qualified Graphics.UI.SDL.Time as Sdl ()
import App
import Util


type AppState = StateT App IO
type AppEnv   = ReaderT AppConfig AppState


main :: IO ()
main = do
  ac <- initAppConfig
  evalStateT (runReaderT loop ac) initApp
  Sdl.quit


loop :: AppEnv ()
loop = do
  t  <- startTicks
  (q, ai)  <- input
  q' <- update ai
  output
  delayApp t
  unless (q || q') loop
  
--

input :: ReaderT AppConfig AppState (Bool, AppInput)
input = loopInput inputApp mkAppInput


loopInput :: MonadIO m => (a -> Sdl.Event -> a) -> a -> m (Bool, a)
loopInput f s = do
  e <- liftIO Sdl.pollEvent
  case e of
    Sdl.Quit    -> return (True, s)
    Sdl.NoEvent -> return (False, s)
    _           -> loopInput f (f s e)


update :: AppInput -> AppEnv Bool
update ai = modify (updateApp ai) >> get >>= \app -> return $ quitApp app


output :: AppEnv ()
output = ask >>= \ac -> get >>= \app -> liftIO $ outputApp ac app


