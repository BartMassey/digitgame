module Main where

import Random
import Data.Maybe
import Data.IORef
import Control.Monad
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
  d1 <- newIORef 1
  d2 <- newIORef 6
  digits <- newIORef [1..9]
  let dice_roller = do
         (d1', d2') <- roll_dice
         imageSetFromPixbuf die_1 (die_images !! (d1' - 1))
         imageSetFromPixbuf die_2 (die_images !! (d2' - 1))
         writeIORef d1 d1'
         writeIORef d2 d2'
  dice_roller
  --- set up rest of boxes
  die_hsep <- hSeparatorNew
  button_box <- hBoxNew True 0
  digit_buttons <- mapM (\i -> toggleButtonNewWithLabel (show i)) [1..9]
  --- the Go button requires special handling
  go_button <- buttonNewWithLabel "Go"
  go_state <- newIORef Nothing
  let fix_go_state new_state = do
         old_state <- readIORef go_state
         case old_state of
           Nothing -> when new_state (do
                        cid <- onClicked go_button dice_roller
                        writeIORef go_state (Just cid))
           Just cid -> unless new_state (do
                         signalDisconnect cid
                         writeIORef go_state Nothing)
  let update_status i = do
         clicked <- mapM (\i -> do
                            a <- toggleButtonGetActive
                                 (digit_buttons !! (i - 1))
                            if a then return i else return 0) [1..9]
         d1' <- readIORef d1
         d2' <- readIORef d2
         fix_go_state (sum clicked == d1' + d2')
  fix_go_state False
  --- wire up the digit buttons
  let set_up_button (b, i) = do
         onToggled b (update_status i)
  mapM_ set_up_button (zip digit_buttons [1..9])
  mapM_ (\b -> boxPackStart button_box b PackGrow 0) digit_buttons
  --- finish the rest of the nesting
  ok_vsep <- vSeparatorNew
  boxPackStart button_box ok_vsep PackGrow 0
  boxPackStart button_box go_button PackGrow 0
  boxPackStart box die_box PackGrow 0
  boxPackStart box die_hsep PackGrow 0
  boxPackStart box button_box PackGrow 0
  --- start the GUI
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
