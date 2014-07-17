module Line where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Cairo

main :: IO ()
main = do
  Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.set window [ Gtk.windowTitle Gtk.:= "Line"
                 , Gtk.windowDefaultWidth Gtk.:= 300
                 , Gtk.windowDefaultHeight Gtk.:= 200
                 , Gtk.containerBorderWidth Gtk.:= 30 ]
  frame <- Gtk.frameNew
  Gtk.containerAdd window frame
  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd frame canvas
  Gtk.widgetModifyBg canvas Gtk.StateNormal $ Gtk.Color 0xffff 0xffff 0xffff
  Gtk.widgetShowAll window 
  drawin <- Gtk.widgetGetDrawWindow canvas
  Gtk.onExpose canvas (\x -> do
                               Gtk.renderWithDrawable drawin drawLine
                               return False)
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.mainGUI

drawLine :: Cairo.Render ()
drawLine = do
  Cairo.setSourceRGB 0 0 0
  Cairo.setLineWidth 5
  Cairo.moveTo 120 60
  Cairo.lineTo 180 110
  Cairo.closePath
  Cairo.stroke

