module Lib where

import System.IO
import System.Random

dup a = (a, a)
undup f (a, b) = f a b
swap (a, b) = (b, a)
clamp (lower, upper) = min upper . max lower

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

(&:) a b = \f -> f a b

-- https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
shuffle x = if length x < 2 then return x else do
	i <- randomRIO (0, length(x)-1)
	r <- shuffle (take i x ++ drop (i+1) x)
	return (x!!i : r)