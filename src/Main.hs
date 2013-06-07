module Main where

import Debug.Trace (traceShow)
import Data.List

f x 
  | c > 60 && c <= 65523  = c
  | otherwise = f c  
  where c = (x + 39887) `mod` 65536
      
crnti = iterate f 10

slice start length x  = take length $ drop start x  

data Event = Hit Int | Miss Int



{-
cache:: Int -> [Event] -> Int -> [Event]
 cache lineSize cachedData addr =
  let cacheAddr = addr `div` lineSize
  in
  map(\Event e ad -> if ad == cacheAddr then Hit cacheAddr else Miss cacheAddr)  
-}


main =
   print $  filter (\x -> length x > 1) $ group $ sort (map (`mod` 4096) (take 2400 crnti)) 

