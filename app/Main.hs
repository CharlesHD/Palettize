module Main where

import Lib
import System.Environment

main :: IO ()
main = do args <- getArgs
          if length args > 1
             then paletteThem (head args) (tail args)
             else putStrLn "Not enough arguments"
