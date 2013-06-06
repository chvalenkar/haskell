module Main where

import           Control.Monad                 (liftM)
import           Data.List                     (sortBy)
import           Debug.Trace
import           Text.ParserCombinators.Parsec


data CacheGrind = CacheGrind {
  i1, d1, ll   :: CacheConf,
  cmd          :: String,
  events       :: [String],
  fileProfiles :: [FileProfile]
} deriving (Show)

data CacheConf = CacheConf {
  name  :: String,
  size  :: Int,
  way   :: Int,
  assoc :: Int
} deriving (Show)


data FileProfile = FileProfile {
  file             :: String,
  functionProfiles :: [FunctionProfile]
} deriving (Show)

data FunctionProfile = FunctionProfile {
  function  :: String,
  lineStats :: [LineStats]
} deriving (Show)

data LineStats = LineStats {
  line  :: Int,
  stats :: [Int]
} deriving (Show)

cacheconf c = do
    string "desc: "; string c; string " cache:"
    spaces
    cachesize <- many digit;  string " B, "; linesize <- many digit; string " B, "
    assoc <- many digit ; string "-way associative\n"
    let r = CacheConf c (read cachesize :: Int) (read linesize :: Int)  (read assoc :: Int)
    return r

parseCmd :: Parser String
parseCmd = do
    string "cmd: "
    manyTill anyChar newline


parseEvents :: Parser [String]
parseEvents = do
    string "events: "
    e <- sepBy1 (many alphaNum) (noneOf "\n")
    newline
    return e


parseFileProfile :: Parser FileProfile
parseFileProfile = do
  try $ string "fl="
  f1 <- manyTill anyChar newline
  f <- many (try parseFunction)
  return FileProfile {file=f1, functionProfiles = f}

parseFunction = do
  string "fn="
  f <- manyTill anyChar newline
  s <- many1 parseStats
  return FunctionProfile { function = f,
                           lineStats = [ LineStats { line = x, stats = xs } | x:xs <- s] }


parseStats :: Parser [Int]
parseStats = do
  d <- sepBy (many1 digit) (char ' ')
  newline
  return $ map read d

parseSummary = do
  string "summary:"
  d<- sepBy (many digit) (char ' ')
  try newline
  return d

header :: Parser CacheGrind
header = do
  [a, b, c] <- mapM cacheconf ["I1", "D1", "LL"]
  d <- parseCmd
  e <- parseEvents
  p <- many parseFileProfile
  parseSummary
  eof
  return CacheGrind { i1 = a, d1 = b, ll = c, cmd = d, events = e, fileProfiles = p}


trace' x = traceShow x x

calcStats x =
  let 
    [_,_,_,a,b,c,d,e,f] = x  
    stalled = (10 * ((b + e) + 10 * (c + f)))            
  in
    stalled - (a +  d)

processLine LineStats { line = l, stats = s } =
  (l, calcStats s)

processFunction FunctionProfile { function = f, lineStats = l } =
  zip (repeat f) (map processLine l)

processFile FileProfile { file = f, functionProfiles = p } =
  zip (repeat f) (concatMap processFunction p)

processCG CacheGrind { fileProfiles = f } =
  let sorted = sortBy (\(_,(_,(_,a))) (_,(_,(_,b))) -> compare b a) (concatMap processFile f)
  in map (\(a,(b,(c,d))) -> (a, b, c, d)) sorted

test = do
  contents <- readFile "data/cachegrind.out.16478"
  case parse header "test" contents of
    Left e -> print e
    Right r -> mapM_ print $ take 50 $ processCG r
  
main = do
  contents <- getContents
  case parse header "test" contents of
    Left e -> print e
    Right r -> mapM_ print $ take 50 $ processCG r
