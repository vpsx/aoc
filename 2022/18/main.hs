-- Day 18 --

import Data.Array
import Data.List

readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)

splitOn :: Char -> String -> [String]
splitOn _ ""     = []
splitOn c s      = let (l, s') = break (== c) s
                     in l : case s' of
                              []        -> []
                              (_:s'')   -> splitOn c s''

main :: IO ()
main
  = do input <- readInputStrs "input.txt"
       let coords' = map (map read) (map (splitOn ',') input) :: [[Int]]
           coords = [ Coord (l!!0) (l!!1) (l!!2) | l <- coords']
           minx = minimum [ x c | c <- coords ] - 1
           miny = minimum [ y c | c <- coords ] - 1
           minz = minimum [ z c | c <- coords ] - 1
           maxx = maximum [ x c | c <- coords ] + 1
           maxy = maximum [ y c | c <- coords ] + 1
           maxz = maximum [ z c | c <- coords ] + 1
           scanBds = (Coord minx miny minz, Coord maxx maxy maxz)
           scanInit = array scanBds (zip (range scanBds) (repeat Air))
           scan = loadscan scanInit coords
           part1answer = exposed scan
           part2answer = outerAirPocket scan
         in do putStrLn ("Part One 4340: " ++ (show part1answer))
               putStrLn ("Part Two 2468: " ++ (show part2answer))


data Coord = Coord { x,y,z :: Int } deriving (Eq, Ord, Ix)

instance Show Coord where show c = show (x c, y c, z c)

data Stuff = Air | Lava | Mystery deriving Eq
type Scan = Array Coord Stuff

loadscan :: Scan -> [Coord] -> Scan
loadscan s cs = foldl (\s c -> s//[(c, Lava)]) s cs

-- Part 1  ¯\_(ツ)_/¯

exposed :: Scan -> Int
exposed s = exp s (assocs s) where
  exp s [] = 0
  exp s ((c, Air):as) = exp s as
  exp s ((c, Lava):as) = countExpFaces s c + exp s as

countExpFaces :: Scan -> Coord -> Int
countExpFaces s c = length [ n | n <- adjacents s c, (s!n) == Air ]

adjacents :: Scan -> Coord -> [Coord]
adjacents s (Coord x y z)
  = filter (inRange (bounds s))
           (map (\(x,y,z) -> Coord x y z)
                [ (x-1, y, z), (x+1, y, z),
                  (x, y-1, z), (x, y+1, z),
                  (x, y, z-1), (x, y, z+1) ])


-- Part 2

-- Count "surface" "area" of outside air "pocket", then subtract grid walls.
-- As problem promises the one and only lava droplet, need not deal with
-- literal edge case where only edges join up but they close up a pocket.

type NegScan = Scan -- It's spiritually different


outerAirPocket :: Scan -> Int
outerAirPocket scan
  = let (start, end) = bounds scan
        initialNegative = array (start, end)
                                 (zip (range (start,end)) (repeat Mystery))
        -- start corner is definitely air bc of how init (+ve) scan was built
        accumulus = initialNegative//[(start, Air)]
        gridwallsurf = gridWallSurface scan
        visitList = adjacents scan start
     in (outer scan accumulus visitList 6) - gridwallsurf  where
  outer :: Scan -> NegScan -> [Coord] -> Int -> Int
  outer posScan negScan [] curSurfArea = curSurfArea
  outer posScan negScan (here:toVisit) curSurfArea
    | canJoinToAccumulus posScan negScan here
      = let (newNeg, newSA) = joinToAccumulus posScan negScan curSurfArea here
            newToVisit = union toVisit (filter (\c -> newNeg!c == Mystery)
                         (adjacents newNeg here))
          in outer posScan newNeg newToVisit newSA
    | (posScan!here == Lava)
      = outer posScan (negScan//[(here, Lava)]) toVisit curSurfArea
    | otherwise
      -- Was air but not joinable; leave it as Mystery in neg scan;
      -- it may become joinable later on
      = outer posScan negScan toVisit curSurfArea


gridWallSurface :: Scan -> Int
gridWallSurface s
  = let ((Coord minx miny minz), (Coord maxx maxy maxz)) = bounds s
        updown    = (maxx-minx+1) * (maxz-minz+1)
        leftright = (maxy-miny+1) * (maxz-minz+1)
        frontback = (maxy-miny+1) * (maxx-minx+1)
      in updown*2+leftright*2+frontback*2


-- A coord can "join" to the accumulating cumulus cloud if:
-- 1. This coord is indeed Air in the positive scan, and
-- 2. One of its adjacents is already Air in the negative scan.
canJoinToAccumulus :: Scan -> NegScan -> Coord -> Bool
canJoinToAccumulus posScan negScan c
  = (posScan!c == Air) && (any (\c -> negScan!c == Air) (adjacents negScan c))


-- Join this coord; update accumulus, surface area
joinToAccumulus :: Scan -> NegScan -> Int -> Coord -> (NegScan, Int)
joinToAccumulus posScan negScan oldSurfArea c
    = (negScan//[(c, Air)],
       oldSurfArea + 6
       + ((-2)*(length (filter (\c -> negScan!c == Air)
                       (adjacents negScan c)))))
