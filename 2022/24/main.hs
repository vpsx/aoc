-- Day 24: Blizzard Fixated --

import Data.Array

main :: IO ()
main
  = do input <- readFile "input.txt"
       let valley = makeValley input
           part1answer = soonest "part1" valley
           part2halfway = soonest "part2reverse" valley
           part2answer = soonest "part2bearingSnacks" valley
         in do putStrLn ("Part One   311: " ++ show part1answer)
               putStrLn ("Part and-a 573: " ++ show part2halfway)
               putStrLn ("Part Two   869: " ++ show part2answer)


type Minute = Int
type Pos = (Int, Int)
type Valley = Array Pos Char

-- Excludes the start and end points (and the walls)
makeValley :: String -> Valley
makeValley input
  = let lns = lines input
        height = length (lns) - 2
        width = length (head lns) - 2
        inner = take height (tail [ take width (tail line) | line <- lns])
        flattened = concat inner
        bounds = ((0,0), (height-1, width-1))
     in array bounds (zip (range bounds) flattened)

startPos v = fst (bounds v)
endPos v = snd (bounds v)
rows v = fst (endPos v) - fst (startPos v) + 1
cols v = snd (endPos v) - snd (startPos v) + 1


{- "Snowically possible" = no blizzards in given spot at given time -}
snowPbl :: Valley -> Minute -> Pos -> Bool
snowPbl v m (r,c)
  =    v!( mod (r+m) (rows v), c ) /= '^' -- 1. no "up" blizzards
    && v!( mod (r-m) (rows v), c ) /= 'v' -- 2. no "down" blizzards
    && v!( r, mod (c+m) (cols v) ) /= '<' -- 3. no "left" blizzards
    && v!( r, mod (c-m) (cols v) ) /= '>' -- 4. no "right" blizzards


{- "Sequentially possible" = snowically possible to be in a neighboring spot
 -  at immediately preceding time, where neighboring spot includes same spot.
 - This got much hairier after the snacks thing... -}
seqPbl :: Bool -> Minute -> ValleyTimeMap -> Valley -> Minute -> Pos -> Bool
-- At minute 0, (pt 2: or whatever,) gang is at start spot(/end spot), off grid
seqPbl _ startMin _ _ m _ | m < (startMin+1) = False
-- First move only depends on snowPblity
seqPbl bkwds _ _ v m p
  | (not bkwds) && p == (startPos v) || bkwds && p == (endPos v)
  = snowPbl v m p
-- Manhat distance (+ 1 for opening move) can't exceed time elapsed
seqPbl bkwds startMin _ v m (r,c)
  | (not bkwds) && r+c+1 > (m-startMin)
    || bkwds && ((rows v)-(r+1))+((cols v)-(c+1))+1 > (m-startMin) --balordo
  = False
seqPbl _ _ vtm v m (r,c)
  = any (\pos -> vtm!(m-1,pos))
        (filter (inRange (bounds v))
                [ (r,c), (r-1,c), (r,c-1), (r+1,c), (r,c+1) ])


type ValleyTimeMap = Array (Minute, Pos) Bool

fix :: (a -> a) -> a
fix f = let { x = f x } in x --duuuud

valleyMem' :: String -> Valley -> Minute -> Minute
              -> ValleyTimeMap -> ValleyTimeMap
valleyMem' leg v minMinutes maxMinutes vtm
  = let bds = ((minMinutes, (fst (bounds v))), (maxMinutes, (snd (bounds v))))
     in (array bds [ ((mn, ps), pbl v mn ps) | (mn, ps) <- range bds ])
    where pbl v m pos
            -- Gave up avoiding magic numbers(
            = if leg == "part1"
              then seqPbl False 0 vtm v m pos && snowPbl v m pos
              else if leg == "part2reverse"
              then seqPbl True 311 vtm v m pos && snowPbl v m pos
              else if leg == "part2bearingSnacks"
              then seqPbl False 573 vtm v m pos && snowPbl v m pos
              else error "sesat ke"

valleyMem :: String -> Valley -> Minute -> Minute -> ValleyTimeMap
valleyMem leg v minMins maxMins = fix (valleyMem' leg v minMins maxMins)

soonest :: String -> Valley -> Minute
soonest leg v
  = let minMins = if leg == "part1" then 0 else
                  if leg == "part2reverse" then 311 else
                  if leg == "part2bearingSnacks" then 573
                  else error "ты куда"
        maxMins = minMins + 400 -- ((
        penultimate = if leg == "part2reverse" then startPos v else endPos v
        memo = valleyMem leg v minMins maxMins
     in 1 + head [ min | min <- [minMins..maxMins], memo!(min,penultimate) ]
