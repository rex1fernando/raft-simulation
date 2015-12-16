{-# LANGUAGE NoMonomorphismRestriction, GADTs, FlexibleContexts #-}
module RaftVisualizer where

import Debug.Trace as D
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Colour.Palette.ColorSet
import Data.Colour.Palette.BrewerSet
import Data.Ratio
import Data.Typeable
import Diagrams.Names
import Diagrams.Util

import DSSimulation (Address, DSEvent, DSState(..), receiver, DSEventType(..))
import Simulation (Event(..), Time)
import RaftSimulation

tshow p x = D.trace (p++(show x)) x

timelines :: [(DSEvent RaftMessage, DSState RaftState () RaftMessage)] -> Diagram Cairo

timelines history =  ((vcat' (with & sep .~ 2) $ 
                      map timeline [0..(num-1)]) === legend # scale 5 # alignL)
                        # applyAll (map link history)
  where
    timeline :: Address -> Diagram Cairo
    timeline a = (position $ map ((eventRect a) . eventTimeColor) 
                           $ filter (forMachine a) history)
                    
              <> alignL (rect 30 1 # fc (colors !! 0) # lw none) 

    num :: Int
    num = length startStates


    eventRect :: Address -> (Simulation.Time,Int) -> (P2 Double, Diagram Cairo)
    eventRect a (time,c) = (p2 (x,0), rect 0.2 1
                                      # fc (bs !! c)  
                                      # lw none 
                                      # named (tshow "" (a,(round time)::Int)))
      where 
        x :: Double
        x = fromRational ((time/duration)*(30%1))
    
    (Event duration _) = (fst . last) history
    dsstate = (snd . head) history
    startStates = machineStates dsstate
    conf = DSSimulation.conf dsstate

eventTimeColor :: (DSEvent RaftMessage, DSState RaftState () RaftMessage)
               -> (Simulation.Time,Int)
eventTimeColor x = etc x
  where
    etc (Event time (Send _ rt), _) = (time, 2*(multiplier rt))
    etc (Event time (Receive _ _ _ rt), _) = (time, 2*(multiplier rt))
    etc (Event time (Crash _), _) = (time, 3)
    
    multiplier (RequestVote _ _) = 0
    multiplier (Voted _ _) = 1
    multiplier (ElectionTimeout) = 2
    multiplier (IAmLeader _ _) = 3
    multiplier (LeaderLost) = 4
    

forMachine :: Address -> (DSEvent RaftMessage, DSState RaftState () RaftMessage) -> Bool
forMachine a' (e,_) = a' == receiver e

legend = node "RequestVote" 0 === node "Voted" 2 
     === node "ElectionTimeout" 4 === node "IAmLeader" 6
     === node "LeaderLost" 8

node t n = (text t # fontSizeL 0.2 # fc black)
         <> ((rect 2 0.2 # fc (bs !! n)) ||| 
            (rect 2 0.2 # fc (bs !! (n+1))))



link :: (DSEvent RaftMessage, DSState RaftState () RaftMessage) -> Diagram Cairo -> Diagram Cairo
link (Event rtime (Receive r s stime _),_) = arrowR (tshow "sender: " (s,stimeD)) (tshow "receiver: " (r,rtimeD))
  where
    stimeD = (round stime) :: Int
    rtimeD = (round rtime) :: Int
link _ = id

arrowR (x,y) (z,w) | (x,y) == (z,w) = id
                  | otherwise = connectOutside' (with  
                                                & headLength .~ local 0.15
                                                & shaftStyle %~ lw ultraThin
                                                ) 
                                                (x,y) (z,w)
--data OrganizedHistory = OrganizedHistory { 
--                          numMachines :: Int,
--                          messages :: [Message]
--                        }
                          
--data Message = Message Address Address (DSState RaftState RaftMessage)

--organize :: [(DSEvent RaftMessage, DSState RaftState RaftMessage)]


-- Color utilities
gr = (1 + sqrt 5) / 2
colors = [d3Colors1 n | n <- [0..10]]
bs = brewerSet Paired 10
bar cs = hcat [square gr # scaleX s # fc k # lwG 0 | k <- cs] # centerXY
  where s = gr / (fromIntegral (length cs))


