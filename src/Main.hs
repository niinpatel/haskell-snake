module Main where

import Graphics.Gloss

width, height, offset :: Int
width = 600

height = 600

offset = 100

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

backgroundColor :: Color
backgroundColor = light $ light $ blue

main :: IO ()
main = display window backgroundColor $ thickCircle 10 80
