{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import Control.Monad.State
import Data.Sequence (Seq, fromList)
import System.Random
import Simulation
import DSSimulation
import RaftSimulation


handler me _ s = do
  put s
  return ([Message (otherServer me) (s++" r")],[])

states = fromList ["",""]
handlers = fromList [handler 0,handler 1]



data MMessage = MMessage Int String deriving Show
data TwoServers = TwoServers String String deriving Show

setText (TwoServers t1 t2) 0 t = TwoServers t t2
setText (TwoServers t1 t2) 1 t = TwoServers t1 t

otherServer x = (x+1)`mod`2

instance EventType MMessage TwoServers where
  process (Event time (MMessage server text)) = do
    state <- get
    put (setText state server text)
    return $ [Event (time+100)
                    (MMessage (otherServer server)
                             (text++" r"))]


    

--main = putStrLn $ show $ take 10 $ simulate [Event 0 (MMessage 0 "")]
--                                            (TwoServers "" "")

showMandS (m,s) = (show m) ++ "\n" ++ (show s)

main :: IO ()
main = do
  randGen <- newStdGen
  mapM_ (putStrLn.showMandS) $ take 10 $ simulateDS [Event 0 (Message 0 "")]
                                         states
                                         handlers
                                         randGen
