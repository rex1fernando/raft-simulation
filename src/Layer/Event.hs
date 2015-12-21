{-# LANGUAGE MultiParamTypeClasses #-}
module Layer.Event where

import Control.Monad.State
import Data.List
import Data.List.Utils

type Time = Integer
type Delta = Integer


data Event t = Event Time t 

addTime :: Time -> Event t -> Event t
addTime time (Event time' t) = Event (time+time') t

instance Eq (Event t) where
  (Event x _) == (Event y _) = x == y
instance Ord (Event t) where
  (Event x _) <= (Event y _) = x <= y


class EventType t s where
  process :: (Show t) => Event t -> State s [Event t]

remainingEvents :: (EventType t s, Show t) => Event t -> [Event t] -> State s [Event t]
remainingEvents event@(Event time t) oldRemaining = do
  newEvents <- map (addTime time) <$> process event
  return $ merge oldRemaining (sort newEvents)
   
  

simulate :: (EventType t s, Show t) => [Event t] -> s -> [(Event t, s)]
simulate [] _ = []
simulate events oldState = (e,newState):simulate remaining newState
  where
    (remaining, newState) = runState (remainingEvents e es) oldState
    (e:es) = sort events
    
