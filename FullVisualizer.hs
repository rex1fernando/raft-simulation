{-# LANGUAGE NoMonomorphismRestriction, GADTs, FlexibleContexts,
    DeriveGeneric #-}
module FullVisualizer where

import Debug.Trace as D
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Colour.Palette.ColorSet
import Data.Colour.Palette.BrewerSet
import Data.Ratio
import Diagrams.Names
import Diagrams.Util

import DSSimulation (Address, DSEvent, DSState(..), receiver, DSEventType(..), machineState)
import Simulation (Event(..), Time)
import RaftFull

class Numerable a where
  variantNum :: a -> Int

instance Numerable RaftMessage where
  variantNum (AppE _ _ _ _ _ _) = 0
  variantNum (AppER _ _ _) = 1
  variantNum (ReqV _ _) = 2
  variantNum (ReqVR _ _) = 3
  variantNum (ElectionTimeout) = 4
  variantNum (HeartbeatTimeout) = 5
  variantNum (AddEntry) = 6

numToVariant 0 = "Crash"
numToVariant 1 = "AppendEntries"
numToVariant 2 = "AppendEntriesR"
numToVariant 3 = "RequestVote"
numToVariant 4 = "RequestVoteR"
numToVariant 5 = "ElectionTimeout"
numToVariant 6 = "HeartbeatTimeout"
numToVariant 7 = "AddEntry"

tshow p x = D.trace (p++(show x)) x

isCrash ((Event _ (Crash _)),_) = True
isCrash _ = False

timelines :: [(DSEvent RaftMessage, DSState RaftState a RaftMessage)] -> Diagram Cairo


historyForMachine history a = filter (forMachine a) history

historyUntilCrash history a = (takeWhile (not . isCrash) $ historyForMachine history a)
                             ++ (filter isCrash $ historyForMachine history a)

timelines history =  (
                       legend # scale 5 # alignL
                       === 
                       strutY 1
                       ===
                       (vcat' (with & sep .~ 2) $ map timeline [0..(num-1)]) 
                     )
                        # applyAll (map link history)
  where
    timeline :: Address -> Diagram Cairo
    timeline a = (position $ map ((eventRect a) . eventTimeStateColor) 
                           $ historyUntilCrash history a)
                    
              <> alignL (rect 30 1 # fc (bs !! 0) # lw none) 


    num :: Int
    num = length startStates


    eventRect :: Address -> (Simulation.Time,Int,[Int],Int) -> (P2 Double, Diagram Cairo)
    eventRect a (time,term,log,c) = (p2 (x,0), (rect 0.2 1
                                      # fc (bs !! c)  
                                      # lw none 
                                      # named (tshow "" (a,(round time)::Int)))
                                      ===
                                      textN (show term)
                                      ===
                                      textN (show log)
                           )
      where 
        x :: Double
        x = fromRational ((time/duration)*(30%1))
    
    (Event duration _) = (fst . last) history
    dsstate = (snd . head) history
    startStates = machineStates dsstate
    conf = DSSimulation.conf dsstate

eventTimeStateColor :: (DSEvent RaftMessage, DSState RaftState a RaftMessage)
               -> (Simulation.Time, Int, [Int], Int)
eventTimeStateColor x = etc x
  where
    etc (Event time (Receive address _ _ rt), s) = (time, currentTerm state, simpleLog state, 2+(variantNum rt))
      where
        state = machineState s address
    etc (Event time (Crash address), s) = (time, currentTerm state, simpleLog state, 1)
      where
        state = machineState s address
    

forMachine :: Address -> (DSEvent RaftMessage, DSState RaftState a RaftMessage) -> Bool
forMachine a' (e,_) = a' == receiver e

legend = vcat $ map legendElement [0..7]

legendElement n = node (numToVariant n) (n+1)

node t n =  rect 0.12 0.12 # fc (bs !! n) # lw none ||| 
            ((text t # fontSizeL 0.10 # fc black) <> rect 1.5 0.12 # fc white # lw none)
            
textN t = ((text t # fontSizeL 0.4 # fc black) <> rect 0.1 0.5 # fc white # lw none)



link :: (DSEvent RaftMessage, DSState RaftState a RaftMessage) -> Diagram Cairo -> Diagram Cairo
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


