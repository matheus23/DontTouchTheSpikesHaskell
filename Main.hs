module Main where

import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Gtk.Window

import FRP.Behaviour

import DTTS.Logic

main :: IO ()
main = runFormBehaviour (0.5, 0.5) gameBehaviour

runFormBehaviour :: (Double, Double) -> Behaviour GtkEvent Form -> IO ()
runFormBehaviour align behaviour = do
  runFormProgram align behaviour step
    where step input currentBehaviour = do
            let newBehaviour = runEvent currentBehaviour input
            return (newBehaviour, value $ newBehaviour)
