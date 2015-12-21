module Scenario.Partition where

import Control.Monad.State

import Layer.Event
import Layer.DS
import Layer.Raft
import Data.Ix
import Data.Ratio

delay = 10
specialDelay = 5

withinPartition :: Address -> Message RaftMessage -> Bool
withinPartition sender (Message receiver t)
  | inRange (0,1) sender && inRange (0,1) receiver = True
  | inRange (2,4) sender && inRange (2,4) receiver = True
  | otherwise = False

sendAllMessages :: Time -> [Message RaftMessage] -> RaftGBehaviorM ()
sendAllMessages time = 
  mapM_ (`send` (time+delay)) 

sendWithinPartition :: Address -> Time -> [Message RaftMessage] -> RaftGBehaviorM ()
sendWithinPartition sender time = 
  mapM_ (`send` (time+delay)) . filter (withinPartition sender)

type RaftGBehaviorM a = GlobalBehaviorM RaftState (Bool,Bool) RaftMessage a
type RaftGBehavior = GlobalBehavior RaftState (Bool,Bool) RaftMessage

global :: RaftGBehavior


global (Event time (Receive sender _ _ _)) ms = do
  (f,s) <- get
  when (time == 80 && not f) $ do
    send (Message 0 AddEntry) (time+5)
    send (Message 0 AddEntry) (time+9)
    put (True,s)
  when (time == 140 && not s) $ do
    send (Message 2 AddEntry) (time+5)
    send (Message 2 AddEntry) (time+9)
    put (f,True)

  if time > 50 && time < 160 then
    sendWithinPartition sender time ms 
    else
      sendAllMessages time ms

global (Event time _) ms = return ()

cb _ = return ()



simulateRaft randGen = Layer.Raft.simulateRaft randGen global (False,False)
