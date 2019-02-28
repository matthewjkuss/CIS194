{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad

import Homework01
import LogAnalysis
import Log

main :: IO ()
main = do 
  mapM_ (\x -> putStrLn $ x) [
      show $ toDigits 1234
    , show $ doubleEveryOther [8,7,6,5]
    , show $ validate 4012888888881881
    , show $ validate 4012888888881882
    , show $ hanoi 3 "a" "b" "c"
    , show $ length $ hanoi 15 "a" "b" "c"
    , show $ parseMessage "E 2 562 help help"
    ]
  void $ testParse parse 10 "src/error.log"
