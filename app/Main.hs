{-# LANGUAGE LambdaCase #-}
module Main where

import IrvSim
import System.Environment

main :: IO ()
main = 
  getArgs >>= \case
    [] -> putStrLn "Usage: ./irvsim ELECTIONFILE"
    (fn:_) -> runElection fn >>= \case
      Nothing -> putStrLn "No one won the election :(\nCheck that your ballots are correctly formatted"
      (Just c) -> putStrLn $ show c ++ " has won the election!"
