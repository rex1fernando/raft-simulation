{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Layer.Raft where

import Prelude hiding (length, replicate, take)
import Data.Ratio
import Data.Maybe (fromMaybe, fromJust, isNothing, Maybe(..))
import Control.Monad
import Control.Monad.State (put, get, State)
import Data.Sequence (fromList, Seq, empty, (><), 
                      replicate, length, index, 
                      update, take, tails, (|>))
import Data.Foldable (toList)
import System.Random
import Layer.Event
import Layer.DS 
import Util


numMachines = 5
quorum = (numMachines `quot` 2) + 1

mesgToAllButMe rm = 
  localhost >>= \l -> mapM_ (`message` rm) $ filter (/= l) [0..4]


data LogEntry = Empty
              | LogEntry { entryIndex :: Int,
                           entryTerm :: Int }
                  deriving Show

lengthen :: Seq LogEntry -> Int -> Seq LogEntry
lengthen s i | length s >= i = s
             | otherwise = s >< replicate (i - length s) Empty

insert :: Seq LogEntry -> Int -> LogEntry -> Seq LogEntry
insert s i a | length s <= i = insert (lengthen s (i+1)) i a
             | otherwise = update i a s

insertAll :: Seq LogEntry -> [LogEntry] -> Seq LogEntry
insertAll = foldl insertAtGivenIndex
  where
    insertAtGivenIndex s' e = insert s' (entryIndex e) e


-- Raft state, message types

data RaftMessage = AppE { term :: Int,      -- AppendEntries
                          leader :: Address,
                          prevLogIndex :: Int,
                          prevLogTerm :: Int,
                          entries :: [LogEntry],
                          leaderCommit :: Int }    
                 | AppER { term :: Int,     -- Response
                           follower :: Address,
                           success :: Bool }
                 | ReqV { term :: Int,             -- RequestVote RPC
                          candidateId :: Address,
                          lastLogIndex :: Int,
                          lastLogTerm :: Int 
                          }
                 | ReqVR { term :: Int,            -- Response
                           voteGranted :: Bool }
                 | ElectionTimeout -- Timer to check whether there is a leader
                 | HeartbeatTimeout -- Timer for leader to heartbeat
                 | AddEntry
                    deriving Show

data RaftState = RaftState { currentTerm :: Int,
                             raftLog :: Seq LogEntry,
                             commitIndex :: Int,
                             lastApplied :: Int,
                             receivedPing :: Bool,
                             
                             nextIndex :: Seq Int,
                             matchIndex :: Seq Int,

                             votesForMe :: Int,
                             votedFor :: Maybe Address,

                             randGen :: StdGen,
                             currentLeader :: Address } deriving Show

nextIndexForM :: RaftState -> Address -> Int
nextIndexForM s = index (nextIndex s)

setNextIndex :: RaftState -> Address -> Int -> RaftState
setNextIndex s a i = s { nextIndex = update a i (nextIndex s) }
setMatchIndex :: RaftState -> Address -> Int -> RaftState
setMatchIndex s a i = s { matchIndex = update a i (matchIndex s) }

logLength :: RaftState -> Int
logLength s = length (raftLog s)

entriesStartingAt :: RaftState -> Int -> [LogEntry]
entriesStartingAt s i = toList $ index (tails (raftLog s)) i

entryAtPos :: RaftState -> Int -> LogEntry
entryAtPos s = index (raftLog s) 

lastEntry :: RaftState -> LogEntry
lastEntry s | logLength s == 0 = LogEntry { entryIndex = -1, entryTerm = -1 }
lastEntry s = index (raftLog s) (logLength s - 1)

append :: RaftState -> Int -> RaftState
append s t = 
 s { raftLog = raftLog s |> LogEntry { entryIndex = logLength s,
                                       entryTerm = t } }

simpleLog :: RaftState -> [Int]
simpleLog s = map entryTerm $ toList (raftLog s)

-- Raft behavior

type RaftHandlerM a = HandlerM RaftState RaftMessage a
type RaftHandler = Handler RaftState RaftMessage

becomeCandidate :: RaftHandlerM ()
becomeCandidate = do
  me <- localhost
  state <- get
  put state { currentTerm = currentTerm state + 1,
              votedFor = Just me,
              votesForMe = 1 }

becomeFollower :: Int -> Address -> RaftHandlerM ()
becomeFollower term leader = do
  me <- localhost
  state <- get
  put state { currentTerm = term,
              votedFor = Nothing,
              votesForMe = 0,
              currentLeader = leader,
              receivedPing = False }

becomeLeader :: RaftHandlerM ()
becomeLeader = do
  me <- localhost
  state <- get
  put state { currentLeader = me }
  sendHeartbeat

sendHeartbeat :: RaftHandlerM ()
sendHeartbeat = do
  me <- localhost
  mapM_ appendEntriesOrHeartbeat $ filter (/=me) [0..numMachines-1]
  setTimeout (70%1) HeartbeatTimeout

updateCommitIndex :: RaftHandlerM ()
updateCommitIndex = return ()

appendEntriesOrHeartbeat = appendEntries_ True
appendEntries = appendEntries_ False

appendEntries_ :: Bool -> Address ->  RaftHandlerM ()
appendEntries_ shouldPing rcvr = do
  me <- localhost
  state <- get

  let rNextIndex = nextIndexForM state rcvr
  let pli = rNextIndex - 1
  let plt = case pli of (-1) -> 0 
                        ; _  -> entryTerm $ entryAtPos state pli

  when (logLength state - 1 >= rNextIndex || shouldPing) $
    message rcvr   AppE { term = currentTerm state,
                          leader = me,
                          prevLogIndex = pli,
                          prevLogTerm = plt,
                          entries = entriesStartingAt state rNextIndex,
                          leaderCommit = commitIndex state }



getRandom :: (Integer,Integer) -> RaftHandlerM Integer

getRandom (lo,hi) = do
  state <- get
  let (val, newRG) = randomR (lo,hi) (randGen state)
  put state { randGen = newRG }
  return val



raftHandler :: RaftHandler
-- ElectionTimeout handler
raftHandler ElectionTimeout = do
  state <- get
  me <- localhost

  -- reset election timeout
  --timeout <- getRandom (150, 300)
  let timeout = (fromIntegral me+1) * 40
  setTimeout (timeout % 1) ElectionTimeout

  if receivedPing state ||
     currentLeader state == me then 
    put state { receivedPing = False }
    else do
      becomeCandidate
      state <- get
      mesgToAllButMe ReqV { term = currentTerm state,
                            lastLogIndex = logLength state - 1,
                            lastLogTerm = entryTerm (lastEntry state),
                            candidateId = me }

-- HeartbeatTimeout handler
raftHandler HeartbeatTimeout =
  sendHeartbeat


-- AddEntry handler
raftHandler AddEntry = do
  me <- localhost 
  state <- get

  put $ append state (currentTerm state)

  mapM_ appendEntries $ filter (/=me) [0..numMachines-1]
  return ()

-- RequestVote handler
raftHandler (ReqV term candidateId lastLogIndex lastLogTerm) = do
  state <- get

  if term < currentTerm state then
    message candidateId ReqVR { term = currentTerm state,
                                voteGranted = False }
    else do
      -- If our term is out of date, reset local election state
      when (term > currentTerm state) $
        becomeFollower term (-1)

      
      -- skipping log up to date check for now
      -- (maybe we can show the protocol failing because of this)
      state <- get
      if isNothing (votedFor state) && 
         candidateUpToDate state lastLogIndex lastLogTerm ||
         (votedFor state == Just candidateId) then 
        message candidateId ReqVR { term = currentTerm state,
                                    voteGranted = True }
        else
          message candidateId ReqVR { term = currentTerm state,
                                      voteGranted = False }

-- Response handler
raftHandler (ReqVR term voteGranted) = do
  state <- get
  if term > currentTerm state then
    becomeFollower term (-1)
    else when voteGranted $ do
      put state { votesForMe = votesForMe state + 1 }
      state <- get
      when (votesForMe state >= quorum) 
        becomeLeader

-- AppendEntries handler
raftHandler (AppE term leader prevLogIndex prevLogTerm entries leaderCommit) = do
  me <- localhost
  state <- get

  -- if receiving message from out of date leader then reject
  if term < currentTerm state then
    message leader AppER { term = currentTerm state,
                            follower = me,
                            success = False }
    else do
      -- else make sure term is up to date with leader
      put state { currentTerm = term,
                  receivedPing = True }
      state <- get

      -- If entries is empty then this is just a ping
      -- don't have to reply
      unless (null entries) $
          if prevLogIndex == (-1)
             || indexSatisfies (raftLog state) prevLogIndex matchesTerm then do
            put state { raftLog = insertAll (take prevLogIndex (raftLog state)) entries }
            state <- get
            when (leaderCommit > commitIndex state) $
              put state { commitIndex = min leaderCommit (length (raftLog state)) }
            message leader AppER { term = currentTerm state,
                                    follower = me,
                                    success = True }

            -- if the log entry before the first new one doesn't match with the 
            -- leader's, then reject
            else 
              message leader AppER { term = currentTerm state,
                                    follower = me,
                                    success = False }
          

  where
    matchesTerm entry = entryTerm entry == prevLogTerm

-- Response handler
raftHandler (AppER term follower success) = do
  state <- get

  if term > currentTerm state then
    becomeFollower term (-1)
    else if success then do
      put $ setNextIndex state follower $ logLength state
      state <- get
      put $ setMatchIndex state follower $ logLength state - 1
      updateCommitIndex
      else do
        put $ setNextIndex state follower $ nextIndexForM state follower - 1 
        appendEntries follower

candidateUpToDate :: RaftState -> Int -> Int -> Bool
candidateUpToDate state cLastIndex cLastTerm -- = True
   | logLength state == 0 = True
   | cLastTerm == entryTerm (lastEntry state) = cLastIndex >= logLength state - 1
   | otherwise = entryTerm (lastEntry state) < cLastTerm
  


cb _ = do
  state <- get
  put $ state {
                commitIndex = 0,
                lastApplied = 0,
                nextIndex = replicate numMachines (logLength state),
                matchIndex = replicate numMachines (-1)
              }


startState rg = RaftState { currentTerm = 0,
                            raftLog = empty,
                            commitIndex = 0,
                            lastApplied = 0,
                            receivedPing = False,
                            nextIndex = replicate numMachines 0,
                            matchIndex = replicate numMachines (-1),
                            votesForMe = 0,
                            votedFor = Nothing,
                            randGen = rg,
                            currentLeader = -1 }
startStates randGen = map startState $ splitN 5 randGen

removeCrashRcvs :: [(DSEvent RaftMessage, DSState RaftState a RaftMessage)] -> [(DSEvent RaftMessage, DSState RaftState a RaftMessage)]
removeCrashRcvs = filter rcvrIsUp
  where
    rcvrIsUp (Event time (Receive a _ _ _),s) = machineIsUp s a
    rcvrIsUp _ = True



simulateRaft :: StdGen -> GlobalBehavior RaftState a RaftMessage -> a -> [(DSEvent RaftMessage, DSState RaftState a RaftMessage)]
simulateRaft randGen global gss = removeCrashRcvs events
  where 
    (r1, r2) = split randGen
    events = simulateDS (DSConf (const raftHandler) global cb)
                                (fromList (startStates r1))
                                gss
                                r2
                                (map (messageEvent ElectionTimeout 0) [0..4])

