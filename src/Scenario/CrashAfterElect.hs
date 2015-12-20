module Scenario.CrashAfterElect where

import Control.Monad.State

import Layer.Event
import Layer.DS
import Layer.Raft


delay = 10 
specialDelay = 5

sendAllMessages :: Time -> [Message RaftMessage] -> RaftGBehaviorM ()
sendAllMessages time = 
  mapM_ (`send` (time+delay)) 

type RaftGBehaviorM a = GlobalBehaviorM RaftState (Bool,Bool) RaftMessage a
type RaftGBehavior = GlobalBehavior RaftState (Bool,Bool) RaftMessage

global :: RaftGBehavior
global (Event time (Receive _ leader  _ (AppE {}))) ms = do
  (alreadyCrashed,r) <- get
  unless alreadyCrashed $ do
    crash leader
    put (not r,r)
  sendAllMessages time ms


global (Event time _) ms = 
  sendAllMessages time ms

cb _ = return ()



simulateRaft recursive randGen = Layer.Raft.simulateRaft randGen global (False,recursive)
