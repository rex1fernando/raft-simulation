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

type RaftHandler = Handler RaftState RaftMessage

-- LeaderLost handler
raftHandler :: RaftHandler
raftHandler LeaderLost = do
  state <- get
  me <- localhost
  let (timeout, newRG) = randomR (150,300) (randGen state)
  put state { currentTerm = ((currentTerm state) + 1),
              votedFor = Just me,
              votesForMe = 1,
              randGen = newRG,
              leaderElected = False}
  mesgToAllButMe (RequestVote ((currentTerm state) + 1) me)
  setTimeout (timeout % 1) ElectionTimeout

-- ElectionTimeout handler
raftHandler ElectionTimeout = do
  state <- get
  me <- localhost
  let (timeout, newRG) = randomR (150,300) (randGen state)
  if leaderElected state then
    return ()
    else do
      put state { currentTerm = ((currentTerm state) + 1),
                  votedFor = Just me,
                  votesForMe = 1,
                  randGen = newRG,  
                  leaderElected = False }
      mesgToAllButMe (RequestVote ((currentTerm state) + 1) me)
      setTimeout (timeout % 1) ElectionTimeout

-- RequestVote handler
raftHandler (RequestVote term address) = do
  state <- get
  me <- localhost
  if (((currentTerm state) == term) && (canVoteFor address state)) || 
     (currentTerm state < term) then do
    put state { currentTerm = term,
                votedFor = Just address,
                votesForMe = 0 }
    message address $ Voted term address
    else
      message address $ Voted (currentTerm state) (fromJust (votedFor state))

-- IAmLeader handler
raftHandler (IAmLeader term address) = do
  state <- get
  me <- localhost
  if ((currentTerm state) <= term) then
    put state { currentTerm = term,
                leaderElected = True }
    else return ()

raftHandler (Voted term address) = do
  state <- get
  me <- localhost
  if (address == me) then do
    put state { votesForMe = (votesForMe state)+1 }
    if ((votesForMe state) >= 3) then do
      put state { leaderElected = True }
      mesgToAllButMe $ IAmLeader (currentTerm state) me
      else
        return ()
    else
      return ()

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


simulateRaft :: StdGen -> [(DSEvent RaftMessage, DSState RaftState () RaftMessage)]
simulateRaft randGen = simulateDS (DSConf (\_ -> raftHandler) global cb)
                                  (fromList (startStates r1))
                                  ()
                                  r2
                                  (map (messageEvent LeaderLost 0) [0..4])
  where (r1, r2) = split randGen
