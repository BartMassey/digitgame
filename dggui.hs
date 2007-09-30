module Main where

import Random
import Graphics.UI.Gtk

--- from the Haskell 98 report
roll_die :: IO Int
roll_die = getStdRandom (randomR (1,6))

roll_dice :: IO (Int, Int)
roll_dice =
    do  r1 <- roll_die
        r2 <- roll_die
        return (r1, r2)

main :: IO ()
main = do
  --- XXX one long function
  --- basic initialization
  initGUI
  window <- windowNew
  box <- vBoxNew True 10
  set window [windowDefaultWidth := 700, windowDefaultHeight := 70,
              containerChild := box, containerBorderWidth := 10]
  --- set up dice
  die_images <- mapM (\i -> pixbufNewFromFile ("gnome-dice-" ++
                                              show i ++ ".svg")) [1..6]
  die_box <- hBoxNew False 0
  die_box_2 <- hBoxNew False 0
  die_1 <- imageNew
  die_2 <- imageNew
  boxPackStart die_box_2 die_1 PackNatural 0
  boxPackStart die_box_2 die_2 PackNatural 0
  boxPackStart die_box die_box_2 PackRepel 0
  let dice_roller = do
         (d1, d2) <- roll_dice
         imageSetFromPixbuf die_1 (die_images !! (d1 - 1))
         imageSetFromPixbuf die_2 (die_images !! (d2 - 1))
  dice_roller
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
