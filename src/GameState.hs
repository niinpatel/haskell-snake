module GameState where

import           Graphics.Gloss.Interface.Pure.Game

data SnakeGame = Game
  {
  }

initialState = Game {}

renderGame _ = thickCircle 10 80

nextFrame _ game = game

handleKeys _ game = game
