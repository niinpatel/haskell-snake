module Main where

import           GameLogic
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Rendering

main :: IO ()
main =
  play window backgroundColor fps initialState renderGame handleKeys nextFrame
  where
    handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = changeDir up game
    handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = changeDir down game
    handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game = changeDir left game
    handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game =
      changeDir right game
    handleKeys (EventKey (Char 'r') _ _ _) _ = initialState
    handleKeys _ game = game
