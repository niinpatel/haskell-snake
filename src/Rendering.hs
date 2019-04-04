module Rendering where

import           GameLogic
import           Graphics.Gloss

width, height, offset :: Int
width = 600

height = 600

offset = 60

window = InWindow "Snake" (width, height) (offset, offset)

backgroundColor = greyN 0.8

fps = 5 :: Int

renderGame game = pictures [snake, food]
  where
    snake = renderFullSnake game
    food = renderFood game

renderSnakeBodyPart location =
  uncurry translate location $
  color ((dark . dark) green) $ rectangleWire snakeSize snakeSize

renderFullSnake game = pictures $ map renderSnakeBodyPart (snakeBody game)

renderFood game =
  uncurry translate (food game) $ color red $ rectangleSolid snakeSize snakeSize
