{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             ExistentialQuantification, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Layer.DS where

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

import Layer.Event
import Layer.DS.Types

-- Global state (config hidden in here too, even though it
-- is not technically state)
data DSState s gs t = DSState { 
                                conf :: (DSConf s gs t)
                              , machineStates :: (Seq s) 
                              , gbState :: gs 
                              }

data DSConf s gs t = DSConf { 
                           machineBehaviors :: MachineBehaviors s t
                         , globalBehavior :: GlobalBehavior s gs t 
                         }

_machineState :: DSState s gs t -> Address -> s
_machineState dsstate address = index (machineStates dsstate) address


data GlobalInfo s t t' = GlobalInfo { _time :: Time,
                                   _conf ::  (DSConf s t) }
                    
newtype GlobalBehaviorM s gs t t' a = GlobalBehaviorM (RandT StdGen 
                                                   (ReaderT GlobalInfo
                                                   (StateT gs 
                                                   (Writer [DSEvent t t']))) a)
                          deriving (Monad, 
                                    Applicative, 
                                    Functor, 
                                    MonadState gs,
                                    MonadWriter [DSEvent t t'],
                                    MonadReader (DSState s gs t),
                                    MonadRandom)

time :: GlobalBehaviorM s gs t Time
time = asks _time 



machineState :: Address -> GlobalBehaviorM s gs t s
machineState address = asks (`_machineState` address)

send :: Message t -> Time -> GlobalBehaviorM s gs t ()
send message time = tell [GSend message time]


runGB :: GlobalBehaviorM s gs t () -> DSState s gs t -> gs -> (gs, [GlobalCommand t])
runGB (GlobalBehaviorM x) rg dsstate gbState = (nrg, newGbState, cmds)
  where 
    (((_, nrg), newGbState), cmds) = runWriter (runStateT (runReaderT (runRandT x rg) dsstate) gbState)

type GlobalBehavior s gs t = [t] -> GlobalBehaviorM s gs t ()

