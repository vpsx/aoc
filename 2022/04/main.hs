-- Day 4 weee

-- Read input line-by-line as strings
readInput :: FilePath -> IO [String]
readInput fp = do input <- readFile fp
                  return (lines input)

splitOn :: Char -> String -> [String]
splitOn _ ""     = []
splitOn c s      = let (l, s') = break (== c) s
                     in l : case s' of
                              []        -> []
                              (_:s'')   -> splitOn c s''


feedAndSumFullyContains :: [Int] -> Int
feedAndSumFullyContains [] = 0
feedAndSumFullyContains (fstLB:fstUB:sndLB:sndUB:xs)
    = fullyContains (fstLB, fstUB) (sndLB, sndUB) + feedAndSumFullyContains xs
feedAndSumFullyContains _ = error "paper jam: need four ints"

-- ill defined who contains whom if e.g. 6-6,6-6 but doesn't matter
fullyContains :: (Int, Int) -> (Int, Int) -> Int
fullyContains (fstLB, fstUB) (sndLB, sndUB)
    = if (fstLB <= sndLB && fstUB >= sndUB)
      || (fstLB >= sndLB && fstUB <= sndUB)
      then 1 else 0

-- part 2

overlapAtAll :: (Int, Int) -> (Int, Int) -> Int
overlapAtAll (fstLB, fstUB) (sndLB, sndUB)
    = if (fstLB <= sndLB && fstUB >= sndLB)
      || (fstLB >= sndLB && sndUB >= fstLB)
      then 1 else 0

feedAndSumOverlaps :: [Int] -> Int
feedAndSumOverlaps [] = 0
feedAndSumOverlaps (fstLB:fstUB:sndLB:sndUB:xs)
    = overlapAtAll (fstLB, fstUB) (sndLB, sndUB) + feedAndSumOverlaps xs
feedAndSumOverlaps _ = error "paper jam: need four ints"


main :: IO ()
main = do elfPairRanges <- readInput "input.txt" -- ["1-2,3-4"]
          let elfRanges = concatMap (splitOn ',') elfPairRanges -- ["1-2","3-4"]
              boundStrs = concatMap (splitOn '-') elfRanges -- ["1","2","3","4"]
              bounds = map read boundStrs -- [1,2,3,4]
              part1answer = feedAndSumFullyContains bounds
              part2answer = feedAndSumOverlaps bounds
            in do putStrLn ("Part One 511: " ++ (show part1answer))
                  putStrLn ("Part Two 821: " ++ (show part2answer))
