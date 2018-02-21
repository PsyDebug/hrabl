module Main where

import Pars
import System.Environment

main :: IO ()
main = do
  ar <- getArgs
  repars $ myarg ar

myarg [] = ""
myarg xs = head xs
