module Main where

import Debug.Trace (traceShow)
import Data.List
import Data.Array


f x 
  | c > 60 && c <= 65523  = c
  | otherwise = f c  
  where c = (x + 39887) `mod` 65536

crnti :: [Int]      
crnti = iterate f 10

slice start length x  = take length $ drop start x  

data CacheLine = CacheLine {
  cachedAddr :: [Int] -- for each way, e.g. 4
} deriving (Show)

data Cache = Cache {
  cache :: [CacheLine] -- for each line - e.g. 65536
} deriving (Show)

data Event = Hit | Miss deriving (Show, Ord, Eq)

emptyLine ways = CacheLine (replicate ways (-1))
emptyCache size ways lineSize = Cache (replicate (size `div` lineSize) (emptyLine ways))

hitOrMissLine lineSize (CacheLine x) addr =   (addr `div` lineSize) `elem` x

hitOrMissCache cacheSize lineSize (Cache x) addr = 
  hitOrMissLine lineSize (x !! lineAddress) addr
  where
    lineAddress = (addr `div` lineSize) `mod` (cacheSize `div` lineSize)
    
putInCacheLine lineSize (CacheLine x) addr =
  let cachedAddr  =  addr `div` lineSize in
  case partition (== cachedAddr) x of 
    ([cachedAddr],miss) -> (Hit,  CacheLine (cachedAddr:miss))
    ([],miss)           -> (Miss, CacheLine (cachedAddr:init miss))
  

putInCache cacheSize lineSize (Cache x) addr =
 (event, Cache (left ++ updatedLine:rest))
  where
    lineAddress = (addr `div` lineSize) `mod` (cacheSize `div` lineSize)
    (left, line:rest) = splitAt lineAddress x
    (event, updatedLine) = putInCacheLine lineSize line addr

data CacheFlow = CacheFlow {
  events :: [Event],
  cached :: Cache
} deriving (Show)

emptyCacheFlow size ways lineSize = CacheFlow [] $ emptyCache size ways lineSize
cacheFlow cacheSize lineSize (CacheFlow e c) addr =
  let 
    (e', c') = putInCache cacheSize lineSize c addr
  in
    CacheFlow (e':e) c' 
      
     
crntiSim = 
 let CacheFlow events  _ = foldl (cacheFlow 65536 128) (emptyCacheFlow 65536 4 128 ) (map (\x -> (x `mod` 2048) *2) $ take 1200 crnti)
 in 
   map length (group  $ sort events)
    
  
  


main =
   print $  filter (\x -> length x > 1) $ group $ sort (map (`mod` 4096) (take 2400 crnti)) 

