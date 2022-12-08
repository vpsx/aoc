-- Day 8: Treehouse: I gave up on FP --

-- Maybe it is time I ventured beyond the Prelude --
-- and learned how ARRAYS WORK in Haskell         --

readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)

main :: IO ()
main
  = do a <- readInputStrs  "input.txt"
       let n = 99
           -- Part 1: very efficient loops (rlud = r-to-l and u-side-d)
           rows = [ [ (read [y], False) | y <-  x ] | x <- a ]
           rowsSeen = map look rows
           rlRows = [ reverse r | r <- rowsSeen ]
           rlRowsSeen = map look rlRows
           rlCols = [[ head (drop i r) | r <- rlRowsSeen] | i <- [0 .. n-1]]
           rlColsSeen = map look rlCols
           rludCols = [ reverse c | c <- rlColsSeen ]
           rludColsSeen = map look rludCols

           part1answer = sum (map countSeen rludColsSeen)

           -- Part 2: _I'm_ still having fun
           rows2 = [ map fst r | r <- rows ]
           cols2 = reverse [ map fst c | c <- rlCols ]

           scenicScores = [scenicScore (rows2, cols2) (x,y)
                          | x<-[0..n-1], y<-[0..n-1]]

           part2answer = maximum scenicScores
         in do putStrLn ("Part One 1681: " ++ (show part1answer))
               putStrLn ("Part Two 201684: " ++ (show part2answer))


-- Part 1

look :: [(Int, Bool)] -> [(Int, Bool)]
-- Outside trees of height 0 should be seen
look ((fstHt, _):rest) = (fstHt, True):(look' fstHt rest) where
    look' _ [] = []
    look' blockedHt ((treeHt, seen):ts)
        = if (treeHt > blockedHt || seen)
          then (treeHt, True):(look' (max treeHt blockedHt) ts)
          else (treeHt, False):(look' blockedHt ts)

countSeen :: [(Int, Bool)] -> Int
countSeen [] = 0
countSeen ((_, seen):ts) = (if seen then 1 else 0) + (countSeen ts)


-- Part 2 I gave up

geti xs i = head (drop i xs)

scenicScore :: ([[Int]],[[Int]]) -> (Int, Int) -> Int
scenicScore (rows, cols) (x,y)
    = vDist2Ways (geti rows x) y
    * vDist2Ways (geti cols y) x

vDist2Ways :: [Int] -> Int -> Int
vDist2Ways [] _ = error "empty list"
vDist2Ways trees n
    = let (left', right') = (take n trees, drop n trees)
          left = reverse left'
          you = head right'
          right = tail right'
      in (countVblTrees left you)*(countVblTrees right you)

-- Can't use takeWhile because... the blocking tree counts
countVblTrees :: [Int] -> Int -> Int
countVblTrees [] _ = 0
countVblTrees (t:ts) you = if t >= you then 1 else 1 + countVblTrees ts you
