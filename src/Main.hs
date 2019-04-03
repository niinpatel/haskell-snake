module Main where

import           GameState
import           Graphics.Gloss

width, height, offset :: Int
width = 600

height = 600

offset = 60

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

backgroundColor :: Color
backgroundColor = light $ light $ blue

fps = 5 :: Int

main :: IO ()
main =
  play window backgroundColor fps initialState renderGame handleKeys nextFrame
