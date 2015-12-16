{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module RaftFull where

import Prelude hiding (length, replicate, take)
import Data.Ratio
import Data.Maybe (fromMaybe, fromJust, isNothing, Maybe(..))
import Control.Monad.State (put, get, State)
import Data.Sequence (fromList, Seq, empty, (><), replicate, length, index, update, take)
import System.Random
import Simulation
import DSSimulation 

-- split Random into n Randoms
splitN 0 randGen = []
splitN n randGen = r1 : splitN (n-1) r2
  where (r1, r2) = split randGen

indexSatisfies :: Seq a -> Int -> (a -> Bool) -> Bool
indexSatisfies s i _ | length s <= i = False
indexSatisfies s i f = f (index s i)


type RaftHandlerM a = HandlerM RaftState RaftMessage a
type RaftHandler = Handler RaftState RaftMessage

numMachines = 5
quorum = (numMachines `quot` 2) + 1

data LogEntry = Empty
              | LogEntry { entryIndex :: Int,
                           entryTerm :: Int }
                  deriving Show

lengthen :: Seq LogEntry -> Int -> Seq LogEntry
lengthen s i | length s >= i = s
             | otherwise = s >< replicate (i-(length s)) Empty

insert :: Seq LogEntry -> Int -> LogEntry -> Seq LogEntry
insert s i a | length s <= i = insert (lengthen s (i+1)) i a
             | otherwise = update i a s

insertAll :: Seq LogEntry -> [LogEntry] -> Seq LogEntry
insertAll s es = foldl insertAtGivenIndex s es
  where
    insertAtGivenIndex s' e = insert s' (entryIndex e) e

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
                          candidateId :: Address
                     --   lastLogIndex :: Int,
                     --   lastLogTerm :: Int 
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
nextIndexForM s a = index (nextIndex s) a

setNextIndex :: RaftState -> Address -> Int -> RaftState
setNextIndex s a i = s { nextIndex = update i a (nextIndex s) }
setNextIndex :: RaftState -> Address -> Int -> RaftState
setMatchIndex s a i = s { matchIndex = update i a (matchIndex s) }

becomeCandidate :: RaftHandlerM ()
becomeCandidate = do
  me <- localhost
  state <- get
  put state { currentTerm = (currentTerm state)+1,
              votedFor = Just me,
              votesForMe = 1 }

becomeFollower :: Address -> RaftHandlerM ()
becomeFollower leader = do
  me <- localhost
  state <- get
  put state { votedFor = Nothing,
              votesForMe = 0,
              currentLeader = leader }

becomeLeader :: RaftHandlerM ()
becomeLeader = do
  me <- localhost
  state <- get
  put state { currentLeader = me }
  sendHeartbeat

sendHeartbeat :: RaftHandlerM ()
sendHeartbeat = do
  me <- localhost
  state <- get
  mesgToAllButMe (AppE { term = (currentTerm state),
                         leader = me,
                         entries = [],
                         prevLogIndex = 0,
                         prevLogTerm = 0,
                         leaderCommit = 0 })
  setTimeout (20%1) HeartbeatTimeout

updateCommitIndex :: RaftHandlerM ()
updateCommitIndex = do
  return ()

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
  timeout <- getRandom (150, 300)
  setTimeout (timeout % 1) ElectionTimeout

  if (receivedPing state) then return ()
    else do
      becomeCandidate
      state <- get
      mesgToAllButMe (ReqV { term = (currentTerm state),
                                    candidateId = me }) 

-- HeartbeatTimeout handler
raftHandler HeartbeatTimeout = do
  sendHeartbeat

-- RequestVote handler
raftHandler (ReqV term candidateId) = do
  state <- get

  if term < (currentTerm state) then
    message candidateId (ReqVR { term = (currentTerm state),
                                 voteGranted = False })
    else do
      -- skipping log up to date check for now
      -- (maybe we can show the protocol failing because of this)
      if (votedFor state == Nothing) ||
         (votedFor state == Just candidateId) then 
        message candidateId (ReqVR { term = (currentTerm state),
                                     voteGranted = True })
        else
          message candidateId (ReqVR { term = (currentTerm state),
                                       voteGranted = False})

-- Response handler
raftHandler (ReqVR term voteGranted) = do
  state <- get
  if term > (currentTerm state) then
    becomeFollower (-1)
    else if voteGranted then do
      put state { votesForMe = (votesForMe state)+1 }
      state <- get
      if (votesForMe state) >= quorum then
        becomeLeader
        else return ()
      else return ()

-- AppendEntries handler
raftHandler (AppE term leader prevLogIndex prevLogTerm entries leaderCommit) = do
  state <- get

  -- if receiving message from out of date leader then reject
  if term < (currentTerm state) then
    message leader (AppER { term = (currentTerm state),
                            success = False })
    else do
      -- else make sure term is up to date with leader
      put state { currentTerm = term }
      state <- get

      -- If entries is empty then this is just a ping
      if null entries then
        put state { receivedPing = True }
        -- don't even have to reply
        else 

          -- if the log entry before the first new one doesn't match with the 
          -- leader's, then reject
          if not (indexSatisfies (raftLog state) prevLogIndex matchesTerm) then
            message leader (AppER { term = (currentTerm state),
                                    success = False })
            -- else do the append
            else do
              put state { raftLog = insertAll (take prevLogIndex (raftLog state)) entries }
              state <- get
              if leaderCommit > (commitIndex state) then
                put state { commitIndex = min leaderCommit (length (raftLog state)) }
                else return ()
              message leader (AppER { term = (currentTerm state),
                                      success = True })
          

  where
    matchesTerm entry = (entryTerm entry) == prevLogTerm

-- Response handler
raftHandler (AppER term follower success) = do
  state <- get
    if term > (currentTerm state) then
      becomeFollower (-1)
      else if success then do
        put $ setNextIndex state follower $ length (raftLog state)
        put $ setMatchIndex state follower $ (length (raftLog state))-1
        updateCommitIndex
        else
          put $ setNextIndex state follower $ (nextIndex state follower)-1
          -- left off here

-- If I haven't voted for anything yet or I've already voted for
-- the requester
canVoteFor :: Address -> RaftState -> Bool
canVoteFor address state =
  (isNothing (votedFor state)) || 
  ((fromMaybe (-1) (votedFor state)) == address)

--toAllBut me rm = map (flip Message rm) $ filter (/= me) [0..4]

mesgToAllButMe rm = 
  localhost >>= \l -> mapM_ (flip message rm) $ filter (/= l) [0..4]


global :: GlobalBehavior RaftState () RaftMessage
global (Event time _) ms = do
  if time > 170 then do
    alreadyCrashed <- getMachineUpStatus 0
    if alreadyCrashed then    
      crash 0
      else return ()
    else return ()
  mapM_ (flip send (time+10)) ms

cb _ = return ()

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

simulateRaft :: StdGen -> [(DSEvent RaftMessage, DSState RaftState () RaftMessage)]
simulateRaft randGen = simulateDS (DSConf (\_ -> raftHandler) global cb)
                                  (fromList (startStates r1))
                                  ()
                                  r2
                                  (map (messageEvent ElectionTimeout 0) [0..4])
  where (r1, r2) = split randGen
