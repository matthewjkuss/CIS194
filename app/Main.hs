{-# OPTIONS_GHC -Wall #-}

module Main where

import Homework01

main :: IO ()
main = mapM_ (\x -> putStrLn $ x) [
    show $ toDigits 1234
  , show $ doubleEveryOther [8,7,6,5]
  , show $ validate 4012888888881881
  , show $ validate 4012888888881882
  ]
