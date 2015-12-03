module TestIf where

import Control.Monad.State


f :: State String ()
f = do
  if True then do
    put "hi"
    return ()
  else
    return ()

main = do
  if True then putStrLn "hi"; else return ()
