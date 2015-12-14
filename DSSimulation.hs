{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             ExistentialQuantification, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module DSSimulation where

import Debug.Trace
import Data.List.Utils
import Data.Sequence
import Data.Traversable
import Data.Ratio
import Data.Foldable
import System.Random (randomR, StdGen)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Random
import Simulation

type Address = Int

-- Events/Event "proxies"
data Receipt t = Receipt Address t Time -- Include time sent
                        deriving Show 
data DSEventType t = Send Address t
                   | Receive Address Address Time t
                   | Crash Address
                   | Restart Address
                        deriving Show

messageEvent t time address = Event time (Receive address address time t)
type DSEvent t = Event (DSEventType t)

receiver (Event _ (Send a _)) = a
receiver (Event _ (Receive a _ _ _)) = a
receiver (Event _ (Crash a)) = a
receiver (Event _ (Restart a)) = a


-- Building the application monad

data Message t = Message Address t 
                        deriving Show
data Timer t = Timer Time t 
                        deriving Show
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
   

message address t = tell $ Outgoing ([Message address t],[])
setTimeout time t = tell $ Outgoing ([],[Timer time t])

-- Running the monad
runHandler :: HandlerM s t a -> Address -> Time -> s -> (a, s, Outgoing t)
runHandler (HandlerM x) address time startState = (a, resultState, outgoing)
  where
    ((a, resultState), outgoing) = runWriter (runStateT (runReaderT x (MachineInfo time address)) startState) 

execHandler :: HandlerM s t a -> Address -> Time -> s -> (s, Outgoing t)
execHandler h address time startState = (resultState, outgoing)
  where
    (_, resultState, outgoing) = runHandler h address time startState 

-- Handler types
type Handler s t = t -> HandlerM s t ()
type Handlers s t = Address -> Handler s t


-- Crash Behavior
type CrashBehavior s t = HandlerM s t ()
type CrashBehaviors s t = Address -> CrashBehavior s t


-- Global state (config hidden in here too, even though it
-- is not technically state)
data DSState s gs t = DSState { 
                                conf :: (DSConf s gs t)
                              , machineStates :: (Seq s) 
                              , upStatus :: (Seq Bool)
                              , gbState :: gs 
                              , deferredMessages :: [Message t]
                              , seed :: StdGen
                              }

makeDSState conf startStates gbStartState randGen =
  DSState conf 
          startStates 
          (Data.Sequence.replicate (Data.Sequence.length startStates) True) 
          gbStartState 
          [] 
          randGen

processGCmds :: Address -> Time -> [GlobalCommand t] -> State (DSState s gs t) [DSEvent t]
processGCmds localhost currentTime cmds = mapM processGCmd cmds >>= return . concat
  where
    processGCmd (GSend (Message address t) time) = return [Event time (Receive address localhost currentTime t)]
    processGCmd (GCrash address) = takeDown address >> return [Event currentTime (Crash address)]
    processGCmd (GTimedCrash address time) = takeDown address >> return  [Event currentTime (Crash address),
                                                                          Event time (Restart address)]
    processGCmd (GRestart address) = do
      es <- takeUp address currentTime
      return ((Event currentTime (Restart address)):es)
    processGCmd (GDefer m) = do
      dsstate <- get
      put dsstate { deferredMessages = m:(deferredMessages dsstate) }
      return []

-- convenience function to get random int and update seed
random :: (Int,Int) -> State (DSState s gs t) Int
random range = do
  dsstate <- get
  let (result, rgNew) = randomR range (seed dsstate)
  put dsstate { seed = rgNew }
  return result

makeTimeoutEvents :: Address -> Time -> [Timer t] -> State (DSState s gs t) [DSEvent t]
makeTimeoutEvents address time timers = return $ map (makeTimeoutEvent address time) timers

makeTimeoutEvent address time (Timer delay t) = Event (time+delay) (Receive address address (time+delay) t)

takeDown :: Address -> State (DSState s gs t) ()
takeDown address = do
  dsstate <- get
  put dsstate { upStatus = (update address False (upStatus dsstate)) }


takeUp :: Address -> Time -> State (DSState s gs t) [DSEvent t]
takeUp address time = do
  dsstate <- get
  let cb = ((crashBehaviors . conf) dsstate)
  let (newState, Outgoing (messages, timers)) = execHandler (cb address) address time (index (machineStates dsstate) address)
  put dsstate { upStatus = (update address False (upStatus dsstate)),
                machineStates = update address newState (machineStates dsstate) }

  ts <- makeTimeoutEvents address time timers

  let gb = ((globalBehavior . conf) dsstate)
  let (nrg, newGbState, cmds) = runGB (gb (Event time (Restart address)) []) 
                                      (seed dsstate)  
                                      (machineStates dsstate)
                                      (gbState dsstate)
  put dsstate { gbState = newGbState, seed = nrg }
  es <- processGCmds address time cmds
  return $ merge ts es 

-- GlobalBehavior
-- takes in a list of messages
-- read machine states, have its own state, generate random numbers,
-- crash and restart machines, send/defer messages/timers

-- Possible commands: send, defer, crash, timed crash, restart
data GlobalCommand t = GSend (Message t) Time 
                     | GCrash Address
                     | GTimedCrash Address Time
                     | GRestart Address
                     | GDefer (Message t)

newtype GlobalBehaviorM s gs t a = GlobalBehaviorM (RandT StdGen 
                                                   (ReaderT (Seq s) 
                                                   (StateT gs 
                                                   (Writer [GlobalCommand t]))) a)
                          deriving (Monad, 
                                    Applicative, 
                                    Functor, 
                                    MonadState gs,
                                    MonadWriter [GlobalCommand t],
                                    MonadReader (Seq s),
                                    MonadRandom)

type GlobalBehavior s gs t = DSEvent t -> [Message t] -> GlobalBehaviorM s gs t ()


getMachineState :: Address -> GlobalBehaviorM s gs t s
getMachineState address = ask >>= \x -> return $ index x address

crash :: Address -> GlobalBehaviorM s gs t ()
crash address = tell [GCrash address]

timedCrash :: Address -> Time -> GlobalBehaviorM s gs t ()
timedCrash address time = tell [GTimedCrash address time]

restart :: Address -> GlobalBehaviorM s gs t ()
restart address = tell [GRestart address]

defer :: Message t -> GlobalBehaviorM s gs t ()
defer message = tell [GDefer message]

deferAll :: [Message t] -> GlobalBehaviorM s gs t ()
deferAll ms = tell (map GDefer ms)

send :: Message t -> Time -> GlobalBehaviorM s gs t ()
send message time = tell [GSend message time]


runGB :: GlobalBehaviorM s gs t () -> StdGen -> Seq s -> gs -> (StdGen, gs, [GlobalCommand t])
runGB (GlobalBehaviorM x) rg machineStates gbState = (nrg, newGbState, cmds)
  where 
    (((_, nrg), newGbState), cmds) = runWriter (runStateT (runReaderT (runRandT x rg) machineStates) gbState)

-- 

-- distributed system configuration
data DSConf s gs t = DSConf { 
                           handlers :: Handlers s t
                         , globalBehavior :: GlobalBehavior s gs t 
                         , crashBehaviors :: CrashBehaviors s t
                         }


-- Put all machine/global handlers/network behavior together
instance (Show t) => EventType (DSEventType t) (DSState s gs t) where
  process e@(Event time (Send _ _)) = return []
  process e@(Event time (Crash _)) = return []
  process e@(Event time (Restart address)) = 
    takeUp address time
  process e@(Event time (Receive address _ _ t)) = do
    dsstate <- get
    if index (upStatus dsstate) address then
      notCrashed
      else
        return []

    where
      notCrashed = do
        dsstate <- get
        let mHandlers = (handlers . conf) dsstate
        
        -- get current state, handler of recipient machine
        let receiverState = index (machineStates dsstate) address
        let handler = mHandlers address

        -- run handler to update state and get timers/outgoing messages
        let (newState, Outgoing (messages, timers)) = handle handler e receiverState

        -- update global state with new machine state
        put dsstate { machineStates = (update address newState (machineStates dsstate)) }
        dsstate <- get

        -- send messages (with global behavior), set timers
        
        let gb = ((globalBehavior . conf) dsstate)
        let (nrg, newGbState, cmds) = runGB (gb e messages) 
                                            (seed dsstate)  
                                            (machineStates dsstate)
                                            (gbState dsstate)
        put dsstate { gbState = newGbState, seed = nrg }
        es <- processGCmds address time cmds

        ts <- makeTimeoutEvents address time timers
        return $ merge es ts

      handle handler (Event time (Receive _ _ _ t)) startState = 
        execHandler (handler t) address time startState


-- interface
simulateDS :: (Show t) =>
              DSConf s gs t
           -> Seq s
           -> gs
           -> StdGen
           -> [DSEvent t] 
           -> [(DSEvent t, DSState s gs t)]
simulateDS conf startStates gbStartState randGen events = 
  simulate events startState
  where
    startState = makeDSState conf startStates gbStartState randGen

-- alternate (better?) definition
--simulateDS events = (flip simulate) . DSState




-------------------------
-- printing

instance (Show s) => Show (DSState s gs t) where
  show dsstate = "State of system:\n"++(showOnLines (machineStates dsstate))
    where
      showOnLines ss = unlines $ toList $ mapWithIndex showOnLine ss
      showOnLine i s = (show i)++": "++(show s)

instance (Show t) => Show (DSEvent t) where
  show (Event time (Send address t)) = 
    "Send: " ++ (show t) ++ " to machine " ++ (show address) ++ " at " ++ (show timeDecimal)
    where
      timeDecimal :: Float
      timeDecimal = fromRational time
  show (Event time (Receive address _ _ t)) = 
    "Receive: " ++ (show t) ++ " to machine " ++ (show address) ++ " at " ++ (show timeDecimal)
    where
      timeDecimal :: Float
      timeDecimal = fromRational time


-- Ideas:
-- 
-- Should have:
--  - Send, Receive, Crash, and Restart events
--  - a function that describes the behavior of the network (i.e. delays,
--  packet drops, partition)
--  - Something for each machine which describes crash behavior
--  - A global handler which observes all events and can initiate crashes
