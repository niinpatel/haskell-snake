module Rendering where

import           GameLogic
import           Graphics.Gloss
import           Utils

tileSize = 30 :: Float

calculateNumberOfTiles (low, high) = abs low + abs high

numberOfTiles = calculateNumberOfTiles $ toFloats gridCoordinatesRange

totalScreenSize = numberOfTiles * tileSize

width, height, offset :: Int
width = toInt totalScreenSize

height = toInt totalScreenSize

offset = 0

window = InWindow "Snake" (width, height) (offset, offset)

backgroundColor = greyN 0.8

fps = 5 :: Int

drawGame :: SnakeGame -> Picture
drawGame game = pictures [snakepicture, foodpicture]
  where
    snakepicture = drawSnakeBody (map getPoint $ snakeBody game)
    foodpicture = drawFood $ getPoint $ food game

drawFood :: Point -> Picture
drawFood = draw red rectangleSolid tileSize

drawSnakeBody :: [Point] -> Picture
drawSnakeBody = pictures . map drawSnakeBodyPart

drawSnakeBodyPart :: Point -> Picture
drawSnakeBodyPart = draw (dark $ dark green) rectangleWire tileSize

type Shape = (Float -> Float -> Picture)

draw :: Color -> Shape -> Float -> Point -> Picture
draw col shape size (x, y) = translate x y $ color col $ shape size size

getPoint :: Location -> Point
getPoint = mapTuple ((+ tileSize / 2) . (* tileSize)) . toFloats
