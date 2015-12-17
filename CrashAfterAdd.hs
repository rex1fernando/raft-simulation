module CrashAfterAdd where

import Control.Monad.State

import RaftFull
import Simulation
import DSSimulation

type RaftGBehaviorM a = GlobalBehaviorM RaftState GlobalState RaftMessage a
type RaftGBehavior = GlobalBehavior RaftState GlobalState RaftMessage

delay = 10 
specialDelay = 5
sendAllMessages time ms = 
  mapM_ (flip send (time+delay)) ms

deviousScheme :: GlobalState -> Time -> Address -> [Message RaftMessage] -> RaftGBehaviorM ()
deviousScheme NothingYet time leader ms = do
  send (Message leader AddEntry) (time+1)
  put $ FirstHeartbeat
  sendAllMessages time ms

deviousScheme FirstHeartbeat time leader ms = do
  put $ AddedOne time
  sendStaggered ms [specialDelay]

  where
    sendStaggered ms delays = mapM_ sendWDelay $ zip delays ms 
    sendWDelay (d,m) = send m $ time+d

deviousScheme (AddedOne startTime) time leader ms = do
  if time == startTime + specialDelay then do
    crash leader
    put $ CrashedLeader
    else return ()

  sendAllMessages time ms

deviousScheme CrashedLeader time leader ms = do
  send (Message leader AddEntry) (time+1)
  put $ AddedOneMore
  sendAllMessages time ms

deviousScheme AddedOneMore time leader ms = do
  sendAllMessages time ms

global :: RaftGBehavior
-- After a leader is elected, try adding to log
global (Event time (Receive _ _ _ (AppE _ leader _ _ _ _))) ms = do
  state <- get
  case state of 
    NothingYet -> deviousScheme state time leader ms
    FirstHeartbeat -> mapM_ (flip send (time+10)) ms
    AddedOne _ -> deviousScheme state time leader ms
    _ -> deviousScheme state time leader ms

global (Event time (Receive leader _ _ AddEntry)) ms = do
  state <- get
  deviousScheme state time leader ms

global (Event time x) ms = do
--  if time > 170 then do
--    alreadyCrashed <- getMachineUpStatus 0
--    if alreadyCrashed then    
--      crash 0
--      else return ()
--    else return ()
  sendAllMessages time ms

cb _ = return ()



simulateRaft randGen = RaftFull.simulateRaft randGen global NothingYet
