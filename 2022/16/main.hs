-- Day 16 --

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map -- I gave up

main :: IO ()
main
  = do input <- readInputStrs "in.txt"
       let (vfs, adjm) = parse input
           nonZeroFlow = filter (\x -> (flowRate x) /= 0) vfs
           distm = distM adjm ("AA":(map fst nonZeroFlow))
           part1yikes = yikes distm nonZeroFlow 30
           part1answer = maximum part1yikes
           part2answer = elephNought distm nonZeroFlow 26
         in do putStrLn ("Part One 1850: " ++ (show part1answer))
               putStrLn ("Part Two 2306: " ++ (show part2answer))


type VName = String
type AdjM = [(VName,VName)] -- Look up BA->AA as ("AA","AB")
type DistM = Map.Map (VName, VName) Int
type VF = (VName, Int) -- Valve, flow rate
type VFS = [VF]


-- Given list of valves (which are supposed to be *nonzero flow* valves)
-- and a number of moves/minutes,
-- return list of possible final totals-pressure-released...
yikes :: DistM -> VFS -> Int -> [Int]
yikes dstm vfs mvsRemain = yikes' dstm ("AA",0) vfs (mvsRemain+1) 0 where
  yikes' :: DistM -> VF -> VFS -> Int -> Int -> [Int]
  yikes' _ _ _ mvs curTotal | mvs <= 0 = [curTotal]
  yikes' dstm curValve unopened mvs curTotal
    -- Opening this valve takes 1 minute, so time it will be open is mvs-1
    = let contribution = (mvs-1)*(flowRate curValve)
          unopenedWDists = [(nxt, distFromM dstm (fst curValve) (fst nxt))
                           | nxt <- unopened]
          onwds = [ yikes' dstm
                           (fst next)
                           (delete (fst next) unopened)
                           (mvs-1-(snd next))
                           (curTotal + contribution)
                    | next <- unopenedWDists,
                      (snd next) < (mvs-1) ] -- ew
        in if onwds == [] then [curTotal + contribution] else (concat onwds)


-- Part 2 v1: did not work
eleph :: DistM -> VFS -> Int -> [Int]
eleph dstm vfs mvsRemain = error "No"

-- Part 2 v2...
elephNought :: DistM -> VFS -> Int -> Int
elephNought dstm vfs minsRemain = elph dstm vfs minsRemain
                                       ("AA",0) ("AA",0)
                                       (minsRemain+1) (minsRemain+1) 0

-- NB "Me" and "Elly" are two entirely fungible bookkeeping entities...
elph :: DistM -> VFS -> Int -> VF -> VF -> Int -> Int -> Int -> Int
elph dstm valvesRmn minsRmn meCurV elCurV meBusyTil elBusyTil curTotal
  | minsRmn <  0        = error "volcano erupted bye"
  | minsRmn == 0        = curTotal
  | valvesRmn == []     = curTotal
  | meBusyTil == 0 && elBusyTil == 0 = curTotal -- vlvs remain, but unreachable
  | meBusyTil >= minsRmn -- I can move
    = case
        [ elph dstm
               (delete nxt valvesRmn)
               -- If Elly also just became free, don't move the time yet...
               (if elBusyTil >= minsRmn then minsRmn else minsRmn-1)
               nxt    -- my current valve: I moved
               elCurV -- Elly's current valve: Elly didn't move
               (minsRmn - nxtDist - 1) -- Me busy until get to nxt and open it
               elBusyTil
               -- Pressure that will be released by this next move
               (curTotal + ((minsRmn-nxtDist-1) * (flowRate nxt)))
        | (nxt,nxtDist) <- nextsDists dstm meCurV valvesRmn,
          -- Only if possible to arrive & open w/at least 1min left
          (minsRmn-nxtDist-1) > 0
        ] of
        -- There are unopened valves left but I cannot get to any of them
        -- but maybe Elly can. Over to Elly (but announce eternal busy-ness)
        [] -> elph dstm valvesRmn
                   -- If Elly also just became free, don't move the time yet...
                   (if elBusyTil >= minsRmn then minsRmn else minsRmn-1)
                   meCurV elCurV 0 elBusyTil curTotal
        onwardsTotals -> maximum onwardsTotals
  | elBusyTil >= minsRmn -- Elly can move
    = case
        [ elph dstm
               (delete nxt valvesRmn)
               -- If I became free at this minute, it was caught above
               (minsRmn-1)
               meCurV -- my current valve: I didn't move
               nxt    -- Elly's current valve: Elly moved
               meBusyTil
               (minsRmn - nxtDist - 1) -- Elly busy til get to nxt and open it
               -- Pressure that will be released by this next move
               (curTotal + ((minsRmn-nxtDist-1) * (flowRate nxt)))
             | (nxt,nxtDist) <- nextsDists dstm elCurV valvesRmn,
               -- Only if possible to arrive & open w/at least 1min left
               (minsRmn-nxtDist-1) > 0
             ] of
        -- There are unopened valves left but Elly cannot get to any of them
        -- but maybe I can. Over to me (but announce Elly eternally busy)
        [] -> elph dstm valvesRmn (minsRmn-1)
                   meCurV elCurV
                   meBusyTil 0 curTotal
        onwardsTotals -> maximum onwardsTotals
  -- Everybody is busy; clock ticks
  | otherwise = elph dstm valvesRmn (minsRmn-1)
                     meCurV elCurV meBusyTil elBusyTil curTotal


nextsDists :: DistM -> VF -> VFS -> [(VF, Int)]
nextsDists dstm cur nexts = [(nxt, distFromM dstm (fst cur) (fst nxt))
                            | nxt <- nexts ]

flowRate :: VF -> Int
flowRate = snd

-- This is the principal cost centre weee
distM :: AdjM -> [VName] -> Map.Map (VName, VName) Int
distM adjm vs = Map.fromList [((a,b), distance adjm a b) | a <- vs, b <- vs]
distFromM dstm x y = fromJust (Map.lookup (x,y) dstm)

distance :: AdjM -> VName -> VName -> Int
distance adjm src dst = d' adjm dst [src] where
  d' adjm dst nxt
    | dst `elem` nxt = 0
    | otherwise      = 1 + d' adjm dst (nextnexts adjm nxt)

nextnexts :: AdjM -> [VName] -> [VName]
nextnexts adjm nxts = nub (concatMap (adjacents adjm) nxts)

adjacents :: AdjM -> VName -> [VName]
adjacents adjm vn = adj' adjm vn where
  adj' [] vn = []
  adj' ((a,b):prs) vn
    | vn == a   = b:(adj' prs vn)
    | vn == b   = a:(adj' prs vn)
    | otherwise = (adj' prs vn)

-- "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
parse :: [String] -> (VFS, AdjM)
parse lines = parse' ([],[]) lines where
parse' :: (VFS, AdjM) -> [String] -> (VFS, AdjM)
parse' (vfs, adjs) [] = (vfs, adjs)
parse' (vfs, adjs) (l:ls)
  = let name = take 2 (drop 6 l)
        (flow,r) = span isDigit (dropWhile (not.isDigit) l)
        adjacents = [take 2 w | w <- drop 5 (words r)]
        adjPairs = [(min name a, max name a) | a <- adjacents]
      in parse' ((name, read flow):vfs, union adjs adjPairs) ls


readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)
