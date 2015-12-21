
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             ExistentialQuantification, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Layer.DS.MachineBehavior where

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

data Request t = Message Address t
               | Timer Time t

data MachineInfo = MachineInfo { _time :: Time, _localhost :: Address }
-- Our application monad
newtype MachineBehaviorM s t a = MachineBehaviorM (ReaderT MachineInfo (StateT s (Writer ([Request t]))) a)
                        deriving (Monad, Applicative, Functor, MonadState s, MonadReader MachineInfo,
                                  MonadWriter ([Request t]))

localhost :: MachineBehaviorM s t Address
localhost = ask >>= (return . _localhost)
time :: MachineBehaviorM s t Time
time = ask >>= (return . _time)
   

message address t = tell $ [Message address t]
setTimeout time t = tell $ [Timer time t]

-- Running the monad
runMB :: MachineBehaviorM s t () -> Address -> Time -> s -> (s, [Request t])
runMB (MachineBehaviorM x) address time startState = (resultState, outgoing)
  where
    ((_, resultState), outgoing) = runWriter (runStateT (runReaderT x (MachineInfo time address)) startState) 


-- Handler types
type MachineBehavior s t = [t] -> MachineBehaviorM s t ()
type MachineBehaviors s t = Address -> MachineBehavior s t

