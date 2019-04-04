module GameLogic where

import           System.Random
import           Utils

type Location = (Float, Float)

type Direction = (Int, Int)

data SnakeGame = Game
  { snakeBody    :: [Location]
  , direction    :: Direction
  , food         :: Location
  , nextFoodList :: [Location]
  }

up, down, left, right :: Direction
up = (0, 1)

down = (0, -1)

left = (-1, 0)

right = (1, 0)

snakeSize = 30 :: Float

initialSnakeBody = [(-195, 285), (-210, 285), (-225, 285)]

initialDirection = right

initialState seed =
  Game
    { snakeBody = initialSnakeBody
    , direction = initialDirection
    , food = head foodList
    , nextFoodList = tail foodList
    }
  where
    foodList = getFoodList seed

getFoodList :: StdGen -> [(Float, Float)]
getFoodList seed = foodListFromRands ((randomRs (-10, 10) seed) :: [Float])
  where
    foodListFromRands (x:y:rest) = foodify (x, y) : (foodListFromRands rest)
    foodify = mapTuple (\x -> (fromIntegral $ floor x) * 30 + 15)

moveSnake game = eatFood $ game {snakeBody = nextLocation}
  where
    currentLocation = snakeBody game
    (headX, headY) = head currentLocation
    (dirX, dirY) = direction game
    newHead =
      teleportThroughWalls $
      ( headX + (fromIntegral dirX * snakeSize)
      , headY + (fromIntegral dirY * snakeSize))
    nextLocation = newHead : init currentLocation

eatFood game
  | eaten =
    growSnake $
    game
      {food = head $ nextFoodList game, nextFoodList = tail $ nextFoodList game}
  | otherwise = game
  where
    eaten = head (snakeBody game) == food game

growSnake game = game {snakeBody = newSnakeBody}
  where
    oldSnakeBody = snakeBody game
    (headX, headY) = head $ oldSnakeBody
    (dirX, dirY) = direction game
    newSnakeBody =
      ( headX + (fromIntegral dirX * snakeSize)
      , headY + (fromIntegral dirY * snakeSize)) :
      oldSnakeBody

checkCollisionWithOwnBody (snakeHead:snakeTail) = elem snakeHead snakeTail

checkGameOver game
  | collidesWithOwnBody =
    game {snakeBody = initialSnakeBody, direction = initialDirection}
  | otherwise = game
  where
    collidesWithOwnBody = checkCollisionWithOwnBody $ snakeBody game

teleportThroughWalls (x, y) = (newX, newY)
  where
    newX
      | x > 300 = -285
      | x < -300 = 285
      | otherwise = x
    newY
      | y > 300 = -285
      | y < -300 = 285
      | otherwise = y

changeDirection (x, y) game = game {direction = updatedDirection}
  where
    updatedDirection =
      if x /= (-1 * (fst $ direction game))
        then (x, y)
        else direction game

nextFrame _ game = checkGameOver $ moveSnake game

keyPressed key game
  | key == "ArrowUp" = changeDirection up game
  | key == "ArrowDown" = changeDirection down game
  | key == "ArrowRight" = changeDirection right game
  | key == "ArrowLeft" = changeDirection left game
  | otherwise = game
