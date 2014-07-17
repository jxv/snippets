module Packing where

import Graphics.UI.Gtk as Gtk

main :: IO ()
main = do
  Gtk.initGUI
  window  <- Gtk.windowNew
  hbox    <- Gtk.hBoxNew True 10
  button1 <- Gtk.buttonNewWithLabel "Button 1"
  button2 <- Gtk.buttonNewWithLabel "Button 2"
  Gtk.set window [ Gtk.windowDefaultWidth Gtk.:= 200
                 , Gtk.windowDefaultHeight Gtk.:= 200
                 , Gtk.containerBorderWidth Gtk.:= 10
                 , Gtk.containerChild Gtk.:= hbox ]
  Gtk.boxPackStart hbox button1 Gtk.PackGrow 0
  Gtk.boxPackStart hbox button2 Gtk.PackGrow 0
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.windowSetTitle window "Template"
  Gtk.mainGUI
