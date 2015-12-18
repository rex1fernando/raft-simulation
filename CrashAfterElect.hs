module CrashAfterElect where

import Control.Monad.State

import RaftFull
import Simulation
import DSSimulation


delay = 10 
specialDelay = 5

sendAllMessages :: Time -> [Message RaftMessage] -> RaftGBehaviorM ()
sendAllMessages time = 
  mapM_ (`send` (time+delay)) 

type RaftGBehaviorM a = GlobalBehaviorM RaftState Bool RaftMessage a
type RaftGBehavior = GlobalBehavior RaftState Bool RaftMessage

global :: RaftGBehavior
global (Event time (Receive _ leader  _ (AppE {}))) ms = do
  alreadyCrashed <- get
  unless alreadyCrashed $ do
    crash leader
    put True
  sendAllMessages time ms


global (Event time _) ms = 
  sendAllMessages time ms

cb _ = return ()



simulateRaft randGen = RaftFull.simulateRaft randGen global False
