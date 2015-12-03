{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import Control.Monad.State
import Data.Sequence (Seq, fromList)
import System.Random
import Simulation
import DSSimulation
import RaftSimulation

showMandS (m,s) = (show m) ++ "\n" ++ (show s)

main = do
  randGen <- newStdGen
  mapM (putStrLn . showMandS) $ simulateRaft randGen
