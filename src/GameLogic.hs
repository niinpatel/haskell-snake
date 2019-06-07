module GameLogic where

import           System.Random (StdGen, randomRs)
import           Utils         (toFloats, toInts)

type Location = (Int, Int)

type Direction = (Int, Int)

type Snake = [Location]

type Food = Location

gridCoordinatesRange :: (Int, Int)
gridCoordinatesRange = (-10, 10) -- a 20 x 20 grid centered around origin

up, down, left, right :: Direction
up = (0, 1)

down = (0, -1)

left = (-1, 0)

right = (1, 0)

initialSnakeBody = [(-7, 9), (-8, 9), (-9, 9)]

initialDirection = right

data SnakeGame = Game
  { snakeBody :: Snake
  , direction :: Direction
  , food      :: Food
  , nextFoods :: [Food]
  }

initialState :: StdGen -> SnakeGame
initialState seed =
  Game
    { snakeBody = initialSnakeBody
    , direction = initialDirection
    , food = head foodList
    , nextFoods = tail foodList
    }
  where
    foodList = getFoods seed

getFoods :: StdGen -> [Food]
getFoods seed = generateFoods (randomRs (toFloats gridCoordinatesRange) seed)
  where
    generateFoods :: [Float] -> [Food]
    generateFoods (x:y:rest) = toInts (x, y) : generateFoods rest

getNextHead :: SnakeGame -> Location
getNextHead game = teleportThroughWalls nextHead
  where
    (headX, headY) = (head . snakeBody) game
    (dirX, dirY) = direction game
    nextHead = (headX + dirX, headY + dirY)

moveSnake :: SnakeGame -> SnakeGame
moveSnake game = game {snakeBody = nextSnakeBody}
  where
    currentSnakeBody = snakeBody game
    nextHead = getNextHead game
    nextSnakeBody = nextHead : init currentSnakeBody

growSnake :: SnakeGame -> SnakeGame
growSnake game = game {snakeBody = nextSnakeBody}
  where
    currentSnakeBody = snakeBody game
    nextHead = getNextHead game
    nextSnakeBody = nextHead : currentSnakeBody

checkFoodEaten :: SnakeGame -> SnakeGame
checkFoodEaten game
  | eaten = (growSnake . eatFood) game
  | otherwise = game
  where
    eaten :: Bool
    eaten = head (snakeBody game) == food game

eatFood :: SnakeGame -> SnakeGame
eatFood game = game {food = head foodList, nextFoods = tail foodList}
  where
    foodList = nextFoods game

checkGameOver :: SnakeGame -> SnakeGame
checkGameOver game
  | collidesWithOwnBody = resetGame game
  | otherwise = game
  where
    collidesWithOwnBody :: Bool
    collidesWithOwnBody = (checkCollisionWithOwnBody . snakeBody) game

changeDirection :: Direction -> SnakeGame -> SnakeGame
changeDirection (x, y) game = game {direction = updatedDirection}
  where
    updatedDirection =
      if x /= (-1 * (fst . direction) game) -- direction should change only orthogonal to its current direction
        then (x, y)
        else direction game

resetGame :: SnakeGame -> SnakeGame
resetGame game =
  game {snakeBody = initialSnakeBody, direction = initialDirection}

checkCollisionWithOwnBody :: Snake -> Bool
checkCollisionWithOwnBody (snakeHead:snakeTail) = snakeHead `elem` snakeTail

teleportThroughWalls :: Location -> Location
teleportThroughWalls (x, y) = (newX, newY)
  where
    minimumNegative = fst gridCoordinatesRange
    maximumPositive = snd gridCoordinatesRange
    newX
      | x >= maximumPositive = minimumNegative
      | x < minimumNegative = maximumPositive - 1
      | otherwise = x
    newY
      | y >= maximumPositive = minimumNegative
      | y < minimumNegative = maximumPositive - 1
      | otherwise = y

nextFrame :: Float -> SnakeGame -> SnakeGame
nextFrame _ = checkGameOver . checkFoodEaten . moveSnake

keyPressed :: String -> SnakeGame -> SnakeGame
keyPressed key
  | key == "ArrowUp" = changeDirection up
  | key == "ArrowDown" = changeDirection down
  | key == "ArrowRight" = changeDirection right
  | key == "ArrowLeft" = changeDirection left
  | key == "r" = resetGame
  | otherwise = id
