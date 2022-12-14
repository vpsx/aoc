-- Day 14: Would not fly on a Raspberry Pi --

import Data.Array

readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)

splitOnStr :: String -> String -> [String]
splitOnStr word s = spl word s "" where
  spl _ "" acc = [(reverse acc)]
  spl word s@(c:cs) acc
    = let l = length word
        in if take l s == word
           then (reverse acc):(spl word (drop l s) "")
           else spl word cs (c:acc)

readCoords :: String -> (Int, Int)
readCoords s = let (r,(comma:d)) = break (== ',') s in (read r, read d)


main :: IO ()
main
  = do input <- readInputStrs "input.txt"
       let coordStringsLists = map (splitOnStr " -> ") input
           coordLists = [ map readCoords l | l <- coordStringsLists ]
           -- But is the universe supposed to end where the rocks end.
           -- Oh right after that is the Endless Void.
           allCoords = concat coordLists
           minRight = minimum (map right allCoords)
           maxRight = maximum (map right allCoords)
           minDown  = 0
           maxDown  = maximum (map down allCoords)
           bounds = ((minRight,minDown),(maxRight,maxDown))
           initSit = array bounds (zip (range bounds) (repeat Air))
           rockySit = foldl renderRock initSit coordLists
           -- Part 1
           (part1answer, part1endSit) = fallAndCountSand rockySit
           -- Part 2
           -- Just +1 and adjust physics instead of adding new rock floor:
           newBounds = ((minRight,minDown),(maxRight,maxDown+1))
           newInitSit = array newBounds (zip (range newBounds) (repeat Air))
           newRockySit = foldl renderRock newInitSit coordLists
           (part2answer, part2endSit) = fallAndCountSandv2 newRockySit
         in do -- To draw the scene:
               --putStrLn (asciiSit part1endSit)
               --putStrLn (asciiSit part2endSit)
               putStrLn ("Part One   737: " ++ (show part1answer))
               putStrLn ("Part Two 28145: " ++ (show part2answer))


data Stuff = Air | Rock | Sand deriving Eq
type Coords = (Int, Int)
type Situation = Array Coords Stuff

right = fst
down = snd

sandSrc = (500,0)

renderRock :: Situation -> [Coords] -> Situation
renderRock s [] = error "No coords to render"
renderRock s (c:cs) = rend s c cs where
  rend s lastPt [] = s
  rend s lastPt (pt:nexts)
    = let vein = range ((min lastPt pt),(max lastPt pt))
        in rend (foldl (\sit coords -> sit//[(coords,Rock)]) s vein) pt nexts


-- Part 1

fallAndCountSand :: Situation -> (Int, Situation)
fallAndCountSand s = fcs (0,s) where
  fcs (i,s) = case sandfall sandSrc s of
                Nothing -> (i,s)
                Just c -> fcs (i+1, s//[(c, Sand)])

-- Given starting coords of sand-unit and current situation,
-- return Just (final resting place) if sand comes to rest,
-- or Nothing if sand falls forever in the void
sandfall :: Coords -> Situation -> Maybe Coords
sandfall (r,d) s
  = let options = [(r,d+1), (r-1, d+1), (r+1, d+1)]
        isUnobstructed = (\c -> (not (inRange (bounds s) c)) || s!c == Air)
        unobstructed = filter isUnobstructed options
      in case unobstructed of
        [] -> Just (r,d) -- Came to rest
        (c:_) -> if (inRange (bounds s) c)
                 then sandfall c s -- Continue falling
                 else Nothing -- Endless Void


-- Part 2

fallAndCountSandv2 :: Situation -> (Int, Situation)
fallAndCountSandv2 s = fcs (0,s) where
  fcs (i,s) = let c = sandfallv2 sandSrc s
                in if c == sandSrc then (i+1,s)
                   else fcs (i+1, (expandSidewaysIfNecessary s c)//[(c, Sand)])


-- Might have made sense to just double the width on each incr;
-- this probably depends on the grid's height:width. Anyway, decided against:
expandSidewaysIfNecessary :: Situation -> Coords -> Situation
expandSidewaysIfNecessary s (r,d)
  = let ((minR,minD),(maxR,maxD)) = bounds s
      in case ((inRange (minD, maxD) d), (inRange (minR, maxR) r)) of
           (False,_) -> error "can't expand down; shouldn't be trying"
           (_,True ) -> s
           -- r will always only be one less/more than current bound
           (_,False) -> if r < minR
                      then array ((r,minD),(maxR, maxD))
                                  ([((r, d'),Air) | d' <- [minD .. maxD]]
                                  ++ (assocs s))
                      else array ((minR,minD),(r, maxD))
                                  ((assocs s)
                                  ++[((r, d'),Air) | d' <- [minD .. maxD]])


sandfallv2 :: Coords -> Situation -> Coords
sandfallv2 (r,d) s
  = let options = [(r,d+1), (r-1, d+1), (r+1, d+1)]
        ((minR,minD),(maxR,maxD)) = bounds s
        -- the new isUnobstructed is: Above new floor, below ceiling,
        -- and is either Air or horizontally-off-grid
        isUnob = (\(r,d) -> (inRange (minD, maxD) d)
                            && ((not (inRange (minR, maxR) r))
                                || s!(r,d) == Air ))
        unobstructed = filter isUnob options
      in case unobstructed of
        [] -> (r,d) -- Came to rest
        (c:_) -> sandfallv2 c s -- Continue falling


-- For fun: Drawing stuff: Input definitely did not disappoint

stuffToChar :: Stuff -> Char
stuffToChar thing = case thing of
                      Air -> '.'
                      Rock -> '#'
                      Sand -> 'o'

asciiSit :: Situation -> String
asciiSit sit = let ((minRight, _),(maxRight, _)) = bounds sit
                   lineLength = maxRight - minRight + 1
                   sigh = transpose sit
                 in breakLines lineLength (map stuffToChar (elems sigh))

transpose :: Array (Int, Int) a -> Array (Int, Int) a
transpose ar
  = let ((a,b),(c,d)) = bounds ar
        newbounds = ((b,a),(d,c))
        newrange = range newbounds
      in array newbounds (zip newrange [ar!(y,x)|(x,y)<-newrange])

breakLines :: Int -> String -> String
breakLines _ [] = []
breakLines n xs = (take n xs) ++ "\n" ++ (breakLines n (drop n xs))
