-- Cigar Cutter GUI
-- based on gtk2hs hello world demo
module Main (Main.main) where

import Graphics.UI.Gtk

contains :: (ContainerClass a, WidgetClass b) => a -> b -> IO ()
contains parent child =
    do
        set parent [ containerChild := child ]
        return ()

wrapAlign :: WidgetClass a =>
                 (Float, Float, Float, Float) -> a -> IO Alignment
wrapAlign (x, y, v, h) w =
    do
        al <- alignmentNew x y v h
        contains al w
        return al

rigidify :: WidgetClass a =>
                 a -> IO Alignment
rigidify = wrapAlign (1, 1, 0, 0)

digitButton :: Int -> IO Button
digitButton d =
    do
        b <- buttonNew
        set b [ buttonLabel := (show d) ]
        onClicked b $ takeDigit d
        return b

pad :: Int
pad = 10


digitButtons :: IO Alignment
digitButtons =
    do
        b <- hBoxNew False 0
        let ds = map digitButton [1..9]
        mapM_ (>>= (contains b)) ds
        a <- wrapAlign (0.5, 0.5, 1, 1) b
        return a

main :: IO ()
main = do
  initGUI
  -- Create a new window
  window <- windowNew
  -- Here we connect the "destroy" event to a signal handler.
  -- This event occurs when we call widgetDestroy on the window
  -- or if the user closes the window.
  onDestroy window mainQuit
  -- Sets the title of the window.
  set window [ windowTitle := "Cigar Cutter" ]
  -- Creates a new box to separate the play area from
  -- the button box
  topBox <- vBoxNew False pad
  -- Creates the button box for the on-screen controls
  buttonBox <- hBoxNew False pad
  -- Creates a new button with the label.
  button <- buttonNew
  set button [ buttonLabel := "Quit" ]
  -- When the button receives the "clicked" signal, it will call the
  -- function given as the second argument.
  -- This one will cause the window to be destroyed by calling
  -- widgetDestroy.
  onClicked button $ do
    widgetDestroy window
  -- Assemble the widget hierarchy
  ab <- rigidify button
  contains buttonBox ab
  abb <- rigidify buttonBox
  db <- digitButtons
  contains topBox db
  contains topBox abb
  contains window topBox
  -- The final step is to display this newly created widget. Note that this
  -- also allocates the right amount of space to the windows and the button.
  widgetShowAll window
  -- All Gtk+ applications must have a main loop. Control ends here
  -- and waits for an event to occur (like a key press or mouse event).
  -- This function returns if the program should finish.
  mainGUI
