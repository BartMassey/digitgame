module Main where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  box <- vBoxNew True 10
  set window [windowDefaultWidth := 700, windowDefaultHeight := 70,
              containerChild := box, containerBorderWidth := 10]
  die_images <- mapM (\i -> imageNewFromFile ("gnome-dice-" ++
                                              show i ++ ".svg")) [1..6]
  die_box <- hBoxNew False 0
  die_box_2 <- hBoxNew False 0
  boxPackStart die_box die_box_2 PackRepel 0
  boxPackStart die_box_2 (die_images !! 0) PackNatural 0
  boxPackStart die_box_2 (die_images !! 5) PackNatural 0
  die_hsep <- hSeparatorNew
  button_box <- hBoxNew True 0
  digit_buttons <- mapM (toggleButtonNewWithLabel . show) [1..9]
  mapM_ (\b -> boxPackStart button_box b PackGrow 0) digit_buttons
  ok_vsep <- vSeparatorNew
  boxPackStart button_box ok_vsep PackGrow 0
  go_button <- buttonNewWithLabel "Go"
  boxPackStart button_box go_button PackGrow 0
  boxPackStart box die_box PackGrow 0
  boxPackStart box die_hsep PackGrow 0
  boxPackStart box button_box PackGrow 0
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
