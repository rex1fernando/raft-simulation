{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module DSSimulation where

import Data.List.Utils
import Data.Sequence
import Data.Traversable
import Data.Ratio
import Data.Foldable
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Simulation

type Address = Int
data Message t = Message Address t deriving Show
data Timer t = Timer Time t deriving Show

type DSEvent t = Event (Message t)

newtype Outgoing t = Outgoing ([Message t], [Timer t])
                                  deriving (Monoid)
data MachineInfo = MachineInfo { _time :: Time, _localhost :: Address }

-- Our application monad
newtype HandlerM s t a = HandlerM (ReaderT MachineInfo (StateT s (Writer (Outgoing t))) a)
                                  deriving (Monad, Applicative, Functor, MonadState s, MonadReader MachineInfo,
                                            MonadWriter (Outgoing t))

localhost :: HandlerM s t Address
localhost = ask >>= (return . _localhost)
time :: HandlerM s t Time
time = ask >>= (return . _time)

message address t = tell ([Message address t],[])
setTimeout time t = tell ([],[Timer time t])

type Handler s t = (Time -> t -> State s ([Message t],[Timer t]))
type Handlers s t = Seq (Handler s t)
data DSState s t = DSState (Seq s) 
                           (Handlers s t) 
                           StdGen



instance (Show s) => Show (DSState s t) where
  show (DSState ss h r) = "State of system:\n"++(showOnLines ss)
    where
      showOnLines ss = unlines $ toList $ mapWithIndex showOnLine ss
      showOnLine i s = (show i)++": "++(show s)

instance (Show t) => Show (DSEvent t) where
  show (Event time (Message address t)) = 
    "Message: " ++ (show t) ++ " to machine " ++ (show address) ++ " at " ++ (show timeDecimal)
    where
      timeDecimal :: Float
      timeDecimal = fromRational time


instance EventType (Message t) (DSState s t) where
  process e@(Event time (Message address t)) = do
    (DSState states handlers randGen) <- get
    let receiverState = index states address
    let handler = index handlers address
    let ((messages, timers), newState) = runState ((handle handler) e) receiverState
    put (DSState (update address newState states)
                 handlers
                 randGen)
    ms <- send messages
    ts <- setTimeouts timers
    return $ merge ms ts

    where
      send :: [Message t] -> State (DSState s t) [DSEvent t]
      send messages = do
        mapM sendWithRandDelay messages
        
      setTimeouts :: [Timer t] -> State (DSState s t) [DSEvent t]
      setTimeouts timers = return $ map setTimeout timers
      
      setTimeout (Timer delay t) = Event (time+delay) (Message address t)
        
      sendWithRandDelay m = do
        (DSState s h randGen) <- get
        let (delay, rgNew) = randomR (80, 160) randGen
        put (DSState s h rgNew)
        return $ Event (time+(delay % 1)) m

      handle handler (Event time (Message _ t)) = 
        handler time t


simulateDS :: [DSEvent t] 
           -> Seq s
           -> Handlers s t
           -> StdGen
           -> [(DSEvent t, DSState s t)]
simulateDS events startStates handlers randGen = 
  simulate events startState
  where
    startState = DSState startStates handlers randGen
