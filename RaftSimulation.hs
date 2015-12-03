{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module RaftSimulation where

import Data.Ratio
import Data.Maybe (fromMaybe, fromJust, isNothing, Maybe(..))
import Control.Monad.State (put, get)
import Data.Sequence (fromList)
import System.Random
import Simulation
import DSSimulation

-- split Random into n Randoms
splitN 0 randGen = []
splitN n randGen = r1 : splitN (n-1) r2
  where (r1, r2) = split randGen

data RaftMessage = RequestVote Int Address -- RequestVote RPC
                 | Voted Int Address -- RequestVote RPC returned
                 | ElectionTimeout -- Timer to check whether there is a leader
                 | IAmLeader Int Address -- First leader heartbeat
                 | LeaderLost 
                    deriving Show

data RaftState = RaftState { currentTerm :: Int,
                             votedFor :: Maybe Address,
                             votesForMe :: Int,
                             randGen :: StdGen,
                             leaderElected :: Bool } deriving Show
startState randGen = RaftState 0 Nothing 0 randGen False
startStates randGen = map startState $ splitN 5 randGen


-- LeaderLost handler
raftHandler :: Address -> Handler RaftState RaftMessage
raftHandler me _ LeaderLost = do
  state <- get
  let (timeout, newRG) = randomR (150000,300000) (randGen state)
  put state { currentTerm = ((currentTerm state) + 1),
              votedFor = Just me,
              votesForMe = 1,
              randGen = newRG,
              leaderElected = False}
  return (toAllBut me (RequestVote ((currentTerm state) + 1) me),
          [Timer (timeout % 1) ElectionTimeout])

-- ElectionTimeout handler
raftHandler me _ ElectionTimeout = do
  state <- get
  let (timeout, newRG) = randomR (150000,300000) (randGen state)
  if leaderElected state then
    return ([],[])
    else do
      put state { currentTerm = ((currentTerm state) + 1),
                  votedFor = Just me,
                  votesForMe = 1,
                  randGen = newRG,  
                  leaderElected = False }
      return (toAllBut me (RequestVote ((currentTerm state) + 1) me),
              [Timer (timeout % 1) ElectionTimeout])

-- RequestVote handler
raftHandler me _ (RequestVote term address) = do
  state <- get
  if (((currentTerm state) == term) && (canVoteFor address state)) || 
     (currentTerm state < term) then do
    put state { currentTerm = term,
                votedFor = Just address,
                votesForMe = 0 }
    return ([Message address (Voted term address)], [])
    else
      return ([Message address (Voted (currentTerm state) (fromJust (votedFor state)))], [])

-- IAmLeader handler
raftHandler me _ (IAmLeader term address) = do
  state <- get
  if ((currentTerm state) <= term) then
    put state { currentTerm = term,
                leaderElected = True }
    else return ()
  return ([],[])

raftHandler me _ (Voted term address) = do
  state <- get
  if (address == me) then do
    put state { votesForMe = (votesForMe state)+1 }
    if ((votesForMe state) >= 3) then do
      put state { leaderElected = True }
      return (toAllBut me (IAmLeader (currentTerm state) me), [])
      else
        return ([],[])
    else
      return ([],[])

-- If I haven't voted for anything yet or I've already voted for
-- the requester
canVoteFor :: Address -> RaftState -> Bool
canVoteFor address state =
  (isNothing (votedFor state)) || 
  ((fromMaybe (-1) (votedFor state)) == address)

toAllBut me rm = map (flip Message rm) $ filter (/= me) [0..4]



handlers = map raftHandler [0..4]


simulateRaft randGen = simulateDS (map ((Event 0) . (flip Message LeaderLost)) [0..4])
                                  (fromList (startStates r1))
                                  (fromList handlers)
                                  r2
  where (r1, r2) = split randGen
