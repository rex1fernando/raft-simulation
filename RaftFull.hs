{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module RaftFull where

import Prelude hiding (length, replicate, take)
import Data.Ratio
import Data.Maybe (fromMaybe, fromJust, isNothing, Maybe(..))
import Control.Monad.State (put, get, State)
import Data.Sequence (fromList, Seq, empty, (><), 
                      replicate, length, index, 
                      update, take, tails, (|>))
import Data.Foldable (toList)
import System.Random
import Simulation
import DSSimulation 
import Util


numMachines = 5
quorum = (numMachines `quot` 2) + 1

mesgToAllButMe rm = 
  localhost >>= \l -> mapM_ (flip message rm) $ filter (/= l) [0..4]


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
setMatchIndex :: RaftState -> Address -> Int -> RaftState
setMatchIndex s a i = s { matchIndex = update i a (matchIndex s) }

logLength :: RaftState -> Int
logLength s = length (raftLog s)

entriesStartingAt :: RaftState -> Int -> [LogEntry]
entriesStartingAt s i = toList $ index (tails (raftLog s)) i

entryAtPos :: RaftState -> Int -> LogEntry
entryAtPos s i = index (raftLog s) i

append :: RaftState -> Int -> RaftState
append s t = 
 s { raftLog = (raftLog s) |> LogEntry { entryIndex = (logLength s),
                                         entryTerm = t } }


-- Raft behavior

type RaftHandlerM a = HandlerM RaftState RaftMessage a
type RaftHandler = Handler RaftState RaftMessage

becomeCandidate :: RaftHandlerM ()
becomeCandidate = do
  me <- localhost
  state <- get
  put state { currentTerm = (currentTerm state)+1,
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
  state <- get
  mesgToAllButMe (AppE { term = (currentTerm state),
                         leader =  me,
                         entries = [],
                         prevLogIndex = 0,
                         prevLogTerm = 0,
                         leaderCommit = 0 })
  setTimeout (70%1) HeartbeatTimeout

updateCommitIndex :: RaftHandlerM ()
updateCommitIndex = do
  return ()

appendEntries :: Address -> RaftHandlerM ()
appendEntries rcvr = do
  me <- localhost
  state <- get

  let rNextIndex = nextIndexForM state rcvr
  let pli = (rNextIndex - 1)
  let plt = case pli of (-1) -> 0 
                        ; _  -> entryTerm $ entryAtPos state (rNextIndex - 1)

  if (logLength state) - 1 >= rNextIndex then do
    message rcvr $ AppE { term = currentTerm state,
                          leader = me,
                          prevLogIndex = pli,
                          prevLogTerm = plt,
                          entries = entriesStartingAt state rNextIndex,
                          leaderCommit = commitIndex state }
    else return ()

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
  let timeout = (fromIntegral (me+1)) * 40
  setTimeout (timeout % 1) ElectionTimeout

  if receivedPing state ||
     currentLeader state == me then 
    put state { receivedPing = False }
    else do
      becomeCandidate
      state <- get
      mesgToAllButMe (ReqV { term = (currentTerm state),
                                    candidateId = me }) 

-- HeartbeatTimeout handler
raftHandler HeartbeatTimeout = do
  sendHeartbeat


-- AddEntry handler
raftHandler AddEntry = do
  me <- localhost 
  state <- get

  put $ append state (currentTerm state)

  mapM appendEntries $ filter (/=me) [0..numMachines-1]
  return ()

-- RequestVote handler
raftHandler (ReqV term candidateId) = do
  state <- get

  if term < (currentTerm state) then
    message candidateId (ReqVR { term = (currentTerm state),
                                 voteGranted = False })
    else do
      -- If our term is out of date, reset local election state
      if term > (currentTerm state) then
        becomeFollower term (-1)
        else return ()

      
      -- skipping log up to date check for now
      -- (maybe we can show the protocol failing because of this)
      state <- get
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
  if term > (currentTerm state) then do
    becomeFollower term (-1)
    else if voteGranted then do
      put state { votesForMe = (votesForMe state)+1 }
      state <- get
      if (votesForMe state) >= quorum then
        becomeLeader
        else return ()
      else return ()

-- AppendEntries handler
raftHandler (AppE term leader prevLogIndex prevLogTerm entries leaderCommit) = do
  me <- localhost
  state <- get

  -- if receiving message from out of date leader then reject
  if term < (currentTerm state) then
    message leader (AppER { term = (currentTerm state),
                            follower = me,
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

          if prevLogIndex == (-1)
             || (indexSatisfies (raftLog state) prevLogIndex matchesTerm) then do
            put state { raftLog = insertAll (take prevLogIndex (raftLog state)) entries }
            state <- get
            if leaderCommit > (commitIndex state) then
              put state { commitIndex = min leaderCommit (length (raftLog state)) }
              else return ()
            message leader (AppER { term = (currentTerm state),
                                    follower = me,
                                    success = True })

            -- if the log entry before the first new one doesn't match with the 
            -- leader's, then reject
            else 
              message leader (AppER { term = (currentTerm state),
                                    follower = me,
                                    success = False })
          

  where
    matchesTerm entry = (entryTerm entry) == prevLogTerm

-- Response handler
raftHandler (AppER term follower success) = do
  state <- get

  if term > (currentTerm state) then
    becomeFollower term (-1)
    else if success then do
      put $ setNextIndex state follower $ length (raftLog state)
      put $ setMatchIndex state follower $ (length (raftLog state)) - 1
      updateCommitIndex
      else do
        put $ setNextIndex state follower $ (nextIndexForM state follower) - 1 
        appendEntries follower


data GlobalState = NothingYet
                 | FirstHeartbeat
                 | AddedOne Time
                 | CrashedLeader
                 | AddedOneMore
                 deriving Show


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

simulateRaft :: StdGen -> (GlobalBehavior RaftState a RaftMessage) -> a -> [(DSEvent RaftMessage, DSState RaftState a RaftMessage)]
simulateRaft randGen global gss = simulateDS (DSConf (\_ -> raftHandler) global cb)
                                             (fromList (startStates r1))
                                             gss
                                             r2
                                             (map (messageEvent ElectionTimeout 0) [0..4])
  where (r1, r2) = split randGen
