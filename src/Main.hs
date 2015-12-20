{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
 ExistentialQuantification #-}
module Main where

import Control.Monad.State
import Data.Sequence (Seq, fromList)
import System.Random
import Layer.DS
import Layer.Raft
import Scenario.CRAfterAdd as CRAfterAdd
import Scenario.CrashAfterAdd as CrashAfterAdd
import Scenario.CrashAfterElect as CrashAfterElect
import Scenario.CrashBadElect as CrashBadElect
import Visualizer
import Data.Ratio
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import System.Environment

showMandS (m,s) = show m ++ "\n" ++ show s


s1 =  (CRAfterAdd.simulateRaft, "interesting/cr_after_add.pdf", 115)
s2 =  (CrashAfterAdd.simulateRaft, "interesting/crash_after_add.pdf", 99)
s3 =  (CrashAfterElect.simulateRaft False, "interesting/crash_after_elect.pdf", 94)
s4 =  (CrashAfterElect.simulateRaft True, "interesting/crash_after_elect_r.pdf", 130)
--s5 =  (CrashBadElect.simulateRaft, "interesting/crash_bad_elect.pdf", 95)
s5 =  (CrashBadElect.simulateRaft, "interesting/crash_bad_elect_fixed.pdf", 109)
 



runScenario (simulate, file, howLong) = do
  let seed = 0

  let t = timelines $ take howLong $ revents seed
  mapM_ (putStrLn . showMandS) $ take howLong $ revents seed
  --let t = timelines [(Event (0%1) (Send 0 LeaderLost), s), (Event (1%1) (Receive 1 0 0 LeaderLost), s)] 
  withArgs ["-o",file,"-w","1000"] $ mainWith $ pad 1.03 $ centerXY t  -- # connectOutside (0::Int,0::Int) (1::Int,1::Int)

  where
    revents s = simulate (mkStdGen s)



main = do
  runScenario s1
  runScenario s2
  runScenario s3
  runScenario s4
  runScenario s5
  --howLong <- getArgs >>= return . read . head



--main = do
--  randGen <- newStdGen
--  --putStrLn $ show $ length $ revents randGen
--  mapM (putStrLn . showMandS) $ take 500 $ revents randGen
--
--  where
--   revents randGen = simulateRaft randGen

--howLong = 150
