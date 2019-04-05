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

initialSnakeBody = [(-7, 9), (-8, 9), (-9, 9)]

initialDirection = right

resetGame game =
  game {snakeBody = initialSnakeBody, direction = initialDirection}

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
    foodListFromRands (x:y:rest) =
      mapTuple floorFloat (x, y) : (foodListFromRands rest)

getNextHead game =
  teleportThroughWalls $
  (headX + (fromIntegral dirX), headY + (fromIntegral dirY))
  where
    (headX, headY) = head $ snakeBody game
    (dirX, dirY) = direction game

moveSnake game = eatFood $ game {snakeBody = nextSnakeBody}
  where
    currentSnakeBody = snakeBody game
    newHead = getNextHead game
    nextSnakeBody = newHead : init currentSnakeBody

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
    currentSnakeBody = snakeBody game
    newHead = getNextHead game
    newSnakeBody = newHead : currentSnakeBody

checkCollisionWithOwnBody (snakeHead:snakeTail) = elem snakeHead snakeTail

checkGameOver game
  | collidesWithOwnBody = resetGame game
  | otherwise = game
  where
    collidesWithOwnBody = checkCollisionWithOwnBody $ snakeBody game

teleportThroughWalls (x, y) = (newX, newY)
  where
    newX
      | x > 9 = -10
      | x < -10 = 9
      | otherwise = x
    newY
      | y > 9 = -10
      | y < -10 = 9
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
  | key == "r" = resetGame game
  | otherwise = game
