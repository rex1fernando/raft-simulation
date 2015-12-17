{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import Control.Monad.State
import Data.Sequence (Seq, fromList)
import System.Random
import Simulation
import DSSimulation
--import RaftSimulation
import CrashAfterElect
import FullVisualizer
import Data.Ratio
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import System.Environment

showMandS (m,s) = (show m) ++ "\n" ++ (show s)

--main = do
--  randGen <- newStdGen
--  --putStrLn $ show $ length $ revents randGen
--  mapM (putStrLn . showMandS) $ take 500 $ revents randGen
--
--  where
--   revents randGen = simulateRaft randGen

--howLong = 150

main = do
  howLong <- getArgs >>= return . read . head
  let seed = 0

  let t = timelines $ take howLong $ revents seed
  mapM (putStrLn . showMandS) $ take howLong $ revents seed
  --let t = timelines [(Event (0%1) (Send 0 LeaderLost), s), (Event (1%1) (Receive 1 0 0 LeaderLost), s)] 
  withArgs ["-o","out.pdf","-w","1000"] $ mainWith $ pad 1.1 t  -- # connectOutside (0::Int,0::Int) (1::Int,1::Int)

  where
    revents s = simulateRaft (mkStdGen s)
