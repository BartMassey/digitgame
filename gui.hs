-- A simple program to demonstrate Gtk2Hs.
module Main (Main.main) where

import Graphics.UI.Gtk

contains :: (ContainerClass a, WidgetClass b) => a -> b -> IO ()
contains parent child =
    do
        set parent [ containerChild := child ]
        return ()

rigidify :: WidgetClass a => a -> IO Alignment
rigidify w =
    do
        al <- alignmentNew 1 1 0 0
        contains al w
        return al

digitButton :: Int -> IO Button
digitButton d =
    do
        b <- buttonNew
        set b [ buttonLabel := (show d) ]
        onClicked b $ do putStrLn (show d)
        return b

pad :: Int
pad = 10


digitButtons :: IO HBox
digitButtons =
    do
        b <- hBoxNew False 0
        let ds = map digitButton [1..9]
        mapM_ (>>= (contains b)) ds
        return b

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
