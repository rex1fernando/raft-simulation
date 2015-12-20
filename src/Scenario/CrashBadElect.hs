module Scenario.CrashBadElect where

import Control.Monad.State

import Layer.Raft
import Layer.Event
import Layer.DS

data GlobalState = NothingYet
                 | FirstHeartbeat
                 | AddedOne Time
                 | CrashedLeader
                 | AddedOneMore
                 deriving Show

type RaftGBehaviorM a = GlobalBehaviorM RaftState GlobalState RaftMessage a
type RaftGBehavior = GlobalBehavior RaftState GlobalState RaftMessage

delay = 10 
specialDelay = 5

sendAllMessages :: Time -> [Message RaftMessage] -> RaftGBehaviorM ()
sendAllMessages time = 
  mapM_ (`send` (time+delay)) 

deviousScheme :: GlobalState -> Time -> Address -> [Message RaftMessage] -> RaftGBehaviorM ()
deviousScheme NothingYet time leader ms = do
  send (Message leader AddEntry) (time+1)
  put FirstHeartbeat
  sendAllMessages time ms

deviousScheme FirstHeartbeat time leader ms = do
  put $ AddedOne time
  --sendStaggered ms [specialDelay]
  mapM_ (`send` (time+specialDelay)) (drop 1 ms)

  where
    sendStaggered ms delays = mapM_ sendWDelay $ zip delays ms 
    sendWDelay (d,m) = send m $ time+d

deviousScheme (AddedOne startTime) time leader ms = do
  when (time == startTime + specialDelay) $ do
    crash leader
    put CrashedLeader

  sendAllMessages time ms

deviousScheme CrashedLeader time leader ms = do
  when (time > 80) $ do
    send (Message leader AddEntry) (time+1)
    put AddedOneMore
  sendAllMessages time ms

deviousScheme AddedOneMore time leader ms = 
  sendAllMessages time ms

global :: RaftGBehavior
-- After a leader is elected, try adding to log
global (Event time (Receive _ _ _ (AppE _ leader _ _ _ _))) ms = do
  state <- get
  case state of 
    NothingYet -> deviousScheme state time leader ms
    FirstHeartbeat -> mapM_ (`send` (time+10)) ms
    AddedOne _ -> deviousScheme state time leader ms
    _ -> deviousScheme state time leader ms

global (Event time (Receive leader _ _ AddEntry)) ms = do
  state <- get
  deviousScheme state time leader ms

global (Event time x) ms =
  sendAllMessages time ms

cb _ = return ()



simulateRaft randGen = Layer.Raft.simulateRaft randGen global NothingYet
