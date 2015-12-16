{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import Control.Monad.State
import Data.Sequence (Seq, fromList)
import System.Random
import Simulation
import DSSimulation
--import RaftSimulation
import RaftVisualizer
import RaftFull
import Data.Ratio
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

showMandS (m,s) = (show m) ++ "\n" ++ (show s)

main = do
  randGen <- newStdGen
  mapM (putStrLn . showMandS) $ take 100 $ simulateRaft randGen


--main = do
--  randGen <- newStdGen
--  let t = timelines $ simulateRaft randGen
--  --let t = timelines [(Event (0%1) (Send 0 LeaderLost), s), (Event (1%1) (Receive 1 0 0 LeaderLost), s)] 
--  mainWith $ pad 1.1 t  -- # connectOutside (0::Int,0::Int) (1::Int,1::Int)
