module GameState where

import           Graphics.Gloss.Interface.Pure.Game

snakeSize = 30 :: Float

data SnakeGame = Game
  { snakePosition :: (Float, Float)
  , direction     :: (Float, Float)
  }

initialState = Game {snakePosition = (-285, 285), direction = (1, 0)}

renderGame game = translate x y $ rectangleSolid snakeSize snakeSize
  where
    (x, y) = snakePosition game

nextFrame _ game = game {snakePosition = nextSnakePosition}
  where
    nextSnakePosition = moveSnake (direction game) (snakePosition game)

moveSnake :: (Float, Float) -> (Float, Float) -> (Float, Float)
moveSnake direction currentPosition = nextPosition
  where
    (dirX, dirY) = direction
    (x, y) = currentPosition
    nextPosition = (x + (dirX * snakeSize), y + (dirY * snakeSize))

handleKeys _ game = game
