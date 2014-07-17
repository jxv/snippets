-- Source: http://hackage.haskell.org/package/hscurses
module Main where

import Control.Exception (bracket_)
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

start = do
  Curses.initScr
  Curses.keypad Curses.stdScr True
  Curses.nl False
  Curses.cBreak True
  Curses.echo True
  hasColors <- Curses.hasColors
  if hasColors
    then do
      Curses.startColor
      Curses.initPair (Curses.Pair 1) (CursesH.black) (CursesH.black)
      Curses.initPair (Curses.Pair 2) (CursesH.green) (CursesH.black)
      Curses.initPair (Curses.Pair 3) (CursesH.red) (CursesH.black)
      Curses.initPair (Curses.Pair 4) (CursesH.cyan) (CursesH.black)
      Curses.initPair (Curses.Pair 5) (CursesH.white) (CursesH.black)
      Curses.initPair (Curses.Pair 6) (CursesH.magenta) (CursesH.black)
      Curses.initPair (Curses.Pair 7) (CursesH.blue) (CursesH.black)
      Curses.initPair (Curses.Pair 8) (CursesH.yellow) (CursesH.black)
      return ()
    else
      return ()
  Curses.wclear Curses.stdScr
  return ()

loop num = do
  c <- Curses.getch
  Curses.attrSet Curses.attr0 (Curses.Pair (mod num 8))
  Curses.refresh
  if Curses.decodeKey c == Curses.KeyChar 'q'
    then return()
    else loop (num + 1)

end = do
  Curses.endWin
  return ()

main = do
  bracket_ start end (loop 0)

