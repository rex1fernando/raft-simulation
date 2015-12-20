module Util where

import Prelude hiding (length, replicate, take)
import Debug.Trace as D
import Data.Sequence (fromList, Seq, empty, (><), 
                      replicate, length, index, 
                      update, take, tails, (|>))
import System.Random


tshow p x = D.trace (p++(show x)) x

-- split Random into n Randoms
splitN 0 randGen = []
splitN n randGen = r1 : splitN (n-1) r2
  where (r1, r2) = split randGen

indexSatisfies :: Seq a -> Int -> (a -> Bool) -> Bool
indexSatisfies s i _ | length s <= i = False
indexSatisfies s i f = f (index s i)

