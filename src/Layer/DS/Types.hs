{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             ExistentialQuantification, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Layer.DS.Types where

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

type Address = Int

data DSEventType t t' = Receive Address Address Time t
                      | Global t'
                       deriving Show

type DSEvent t t' = Event (DSEventType t t')

sender (Event _ (Receive _ a _ _)) = a
receiver (Event _ (Receive a _ _ _)) = a
receiveTime (Event time (Receive {})) = time
sendTime (Event _ (Receive _ _ time _)) = time


