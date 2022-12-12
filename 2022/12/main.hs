-- Day 12: upon which more swathes of the std libs are discovered --

import Data.Array
import Data.List  --whoa
import Data.Maybe --whooaaa

main :: IO ()
main
  = do input <- readFile "input.txt"
       let gridht = length (lines input)
           gridwt = length (head (lines input))
           elegrid = array ((0,0),(gridht-1, gridwt-1))
                           (zip (range ((0,0),(gridht-1, gridwt-1)))
                                (concat (words input))) --strip \n lol
           distmap = distMap elegrid
           part1answer = fromJust (distmap!(startPos elegrid))
           -- Part 2
           aSquares = filter (\x -> snd x == 'a') (assocs elegrid)
           aSqPosns = [distmap!(fst x) | x <- aSquares]
           part2answer = minimum (catMaybes aSqPosns)
         in do putStrLn ("Part One 437: " ++ (show part1answer))
               putStrLn ("Part Two 430: " ++ (show part2answer))


type Pos = (Int, Int)
type EleMap = Array Pos Char
type DistMap = Array Pos (Maybe Int)

eleDict = array ('a','z') (zip ['a' .. 'z'] [0..26])

getElev :: Char -> Int
getElev 'S' = eleDict!'a'
getElev 'E' = eleDict!'z'
getElev  c  = eleDict!c

accessible :: EleMap -> Pos -> Pos -> Bool
accessible em src dst =  (getElev (em!dst) - getElev (em!src)) <= 1

findCharPos :: EleMap -> Char -> Pos
findCharPos em c = sc (assocs em) where
  sc [] = error "Char not found in map"
  sc ((pos,elev):prs) = if elev == c then pos else sc prs

startPos em = findCharPos em 'S'
endPos em = findCharPos em 'E'

udlr :: (Pos, Pos) -> Pos -> [Pos]
udlr rng (r,c) = filter (inRange rng) [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

seen, unseen :: DistMap -> Pos -> Bool
seen dm p = isJust (dm!p)
unseen dm p = isNothing (dm!p)

--dies if empty list but that's ok
getMinDist :: DistMap -> [Pos] -> Int
getMinDist dm ps = minimum (mapMaybe (dm!) ps)

-- DistMap shows the shortest dist from each sqare to the 'E' square, or
-- Nothing if there is no (gearlessly-)climbable route from there to 'E'.
distMap :: EleMap -> DistMap
distMap em
  = let initdmap' = array (bounds em) [(i,Nothing) | i <- (indices em)]
        end = endPos em
        initdmap = initdmap'//[(end, Just 0)]
        initVisitNext = udlr (bounds em) end
      in dmap initdmap initVisitNext where
  dmap :: DistMap -> [Pos] -> DistMap
  dmap dm [] = dm
  dmap dm (p:ps)
    = let adjacents = udlr (bounds dm) p
          adjSeen = filter (seen dm) adjacents
          adjAccbl = filter (accessible em p) adjSeen
        in case adjAccbl of
             -- No route to End from here; simply move on
             [] -> dmap dm ps
             -- One or more routes from here to End; rec own minimum dist
             -- and add unvisited adjacents to visit list
             a -> dmap (dm//[(p, Just ((getMinDist dm a)+1))])
                       -- Swap out union for ++ and watch world explode!
                       (union ps (filter (unseen dm) adjacents))
