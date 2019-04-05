module Rendering where

import           GameLogic
import           Graphics.Gloss

gridSize = 30 :: Float

width, height, offset :: Int
width = 600

height = 600

offset = 0

window = InWindow "Snake" (width, height) (offset, offset)

backgroundColor = greyN 0.8

fps = 5 :: Int

renderGame game = pictures [snake, food]
  where
    snake = renderFullSnake game
    food = renderFood game

renderSnakeBodyPart (x, y) =
  translate ((x * gridSize) + 15) ((y * gridSize) + 15) $
  color ((dark . dark) green) $ rectangleWire gridSize gridSize

renderFullSnake game = pictures $ map renderSnakeBodyPart (snakeBody game)

renderFood game =
  translate ((x * gridSize) + 15) ((y * gridSize) + 15) $
  color red $ rectangleSolid gridSize gridSize
  where
    (x, y) = food game
