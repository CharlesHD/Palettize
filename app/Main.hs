module Main where

import Lib
import System.Environment

main :: IO ()
main = do args <- getArgs
          if length args > 1
             then paletteThem' (args !! 0) (args !! 1)
             else paletteThem' "palette.bmp" "."
