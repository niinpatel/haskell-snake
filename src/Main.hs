module Main where

import           GameLogic
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Rendering
import           System.Random

main :: IO ()
main =
  play
    window
    backgroundColor
    fps
    initialState
    renderGame
    handleInputEvents
    nextFrame
  where
    handleInputEvents (EventKey (SpecialKey KeyUp) _ _ _) game =
      keyPressed "ArrowUp" game
    handleInputEvents (EventKey (SpecialKey KeyDown) _ _ _) game =
      keyPressed "ArrowDown" game
    handleInputEvents (EventKey (SpecialKey KeyLeft) _ _ _) game =
      keyPressed "ArrowLeft" game
    handleInputEvents (EventKey (SpecialKey KeyRight) _ _ _) game =
      keyPressed "ArrowRight" game
    handleInputEvents (EventKey (Char 'r') _ _ _) game = keyPressed "r" game
    handleInputEvents _ game = game
