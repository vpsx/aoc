-- Day 9 --

readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)

main :: IO ()
main
  = do x <- readInputStrs "input.txt"
       let mvs = [(head ln, read (head (tail ln))::Int) | ln <- map words x]
           singleMvs = concat [ replicate (snd x) (fst x) | x <- mvs]
           -- Part 1: Let's start at origin
           headPosns = scanl mvHead (Pos 0 0) singleMvs
           tailPosns = scanl mvTail (Pos 0 0) headPosns
           part1answer = length (poorMansSet tailPosns)
           -- Part 2: omg really
           whipAround = scanl mvTail (Pos 0 0)
           knotNPosns = (iterate whipAround headPosns) -- so H,1,2,3...
           knot9Posns = head (drop 9 knotNPosns)
           part2answer = length (poorMansSet knot9Posns)
         in do putStrLn ("Part One 6087: " ++ (show part1answer))
               putStrLn ("Part Two 2493: " ++ (show part2answer))


data Pos = Pos { x, y :: Int} deriving Eq


-- Diff LTE 1
dLTE1 a b = (a-b)*(a-b) <= 1
-- Unit direction difference
udd a b = quot (a-b) (abs (a-b))


-- Given tail Pos and head Pos return tail Pos
mvTail :: Pos -> Pos -> Pos
mvTail t h
  -- Are already touching
  | dLTE1 (x h) (x t) && dLTE1 (y h) (y t) = t
  -- Are not touching, are in line
  | x t == x h = Pos (x t)
                     ((y t) + udd (y h) (y t))
  | y t == y h = Pos ((x t) + udd (x h) (x t))
                     (y t)
  -- Are not touching, are not in line
  | otherwise  = Pos ((x t) + udd (x h) (x t))
                     ((y t) + udd (y h) (y t))


-- Given direction and (head) Pos return (head) Pos
mvHead :: Pos -> String -> Pos
mvHead p d
  | d == "U" = Pos (x p) (y p + 1)
  | d == "D" = Pos (x p) (y p - 1)
  | d == "L" = Pos (x p - 1) (y p)
  | d == "R" = Pos (x p + 1) (y p)


-- Wow this is terrible
poorMansSet :: [Pos] -> [Pos]
poorMansSet [] = []
poorMansSet (p:ps) = if elem p ps
                     then poorMansSet ps
                     else p:(poorMansSet ps)
