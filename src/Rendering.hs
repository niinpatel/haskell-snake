module Rendering where

import           GameLogic
import           Graphics.Gloss

width, height, offset :: Int
width = 600

height = 600

offset = 60

window = InWindow "Snake" (width, height) (offset, offset)

backgroundColor = light $ light $ blue

fps = 5 :: Int

renderGame game = pictures [snake, snakeFood]
  where
    snake = renderFullSnake (snakeBody game)
    snakeFood = renderFood (food game)

renderSnakeBodyPart (x, y) = translate x y $ rectangleSolid snakeSize snakeSize

renderFullSnake snakeBody = pictures $ map renderSnakeBodyPart snakeBody

renderFood (x, y) = translate x y $ rectangleSolid snakeSize snakeSize
