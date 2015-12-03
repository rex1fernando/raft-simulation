{-# LANGUAGE MultiParamTypeClasses #-}
module Simulation where

import Control.Monad.State
import Data.List
import Data.List.Utils

type Time = Rational
data Event t = Event Time t
-- Shouldn't ever make two events with the same time but different types
instance Eq (Event t) where
  (Event x _) == (Event y _) = x == y
instance Ord (Event t) where
  (Event x _) <= (Event y _) = x <= y


class EventType t s where
  process :: Event t -> State s [Event t]




remainingEvents :: (EventType t s) => Event t -> [Event t] -> State s [Event t]
remainingEvents event oldRemaining = do
  newEvents <- process event
  return $ merge oldRemaining (sort newEvents)
   
  

simulate :: (EventType t s) => [Event t] -> s -> [(Event t, s)]
simulate [] _ = []
simulate events oldState = (e,newState):(simulate remaining newState)
  where
    (remaining, newState) = runState (remainingEvents e es) oldState
    (e:es) = sort events
    
