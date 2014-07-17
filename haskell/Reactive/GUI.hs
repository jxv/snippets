module GUI where

import Control.Monad (void)
import Graphics.UI.Gtk
import FRP.Sodium


main :: IO ()
main = undefined



gui :: IO ()
gui =
  do (evt, send) <- sync newEvent :: IO (Event String, String -> Reactive ())
     initGUI
     win <- windowNew
     hbox <- hBoxNew True 10
     btn1 <- buttonNewWithLabel "Btn 1"
     btn2 <- buttonNewWithLabel "Btn 2"
     set win $
       [ windowDefaultWidth := 200
       , windowDefaultHeight := 200
       , containerBorderWidth := 10
       , containerChild := hbox ]
     boxPackStart hbox btn1 PackGrow 0
     boxPackStart hbox btn2 PackGrow 0
     void $ onButtonPress btn1 $ \e ->
       do sync $ send (show e)
          return False
     onDestroy win mainQuit
     widgetShowAll win
     windowSetTitle win "title"
     killListener <- sync $ listen evt putStrLn
     mainGUI
     killListener

