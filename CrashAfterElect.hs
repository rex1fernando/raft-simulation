module CrashAfterElect where

import Control.Monad.State

import RaftFull
import Simulation
import DSSimulation


delay = 10 
specialDelay = 5
sendAllMessages time ms = 
  mapM_ (flip send (time+delay)) ms

type RaftGBehaviorM a = GlobalBehaviorM RaftState Bool RaftMessage a
type RaftGBehavior = GlobalBehavior RaftState Bool RaftMessage

global :: RaftGBehavior
global (Event time (Receive _ leader  _ (AppE _ _ _ _ _ _))) ms = do
  alreadyCrashed <- get
  if not alreadyCrashed then do
    crash leader
    put True
    else return ()
  sendAllMessages time ms


global (Event time _) ms = do
  sendAllMessages time ms

cb _ = return ()



simulateRaft randGen = RaftFull.simulateRaft randGen global False
