module Lib where

import System.IO

dup a = (a, a)
undup f (a, b) = f a b
swap (a, b) = (b, a)
clamp (lower, upper) = min upper . max lower

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing
