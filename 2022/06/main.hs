-- Day 6

main :: IO ()
main
  = do x <- readInputStrs "input.txt"
       let s = head x
           part1answer = start s 4
           part2answer = start s 14
         in do putStrLn ("Part One 1766: " ++ (show part1answer))
               putStrLn ("Part Two 2383: " ++ (show part2answer))

-- Read input, splitting on newlines (Strings)
readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)


start :: String -> Int -> Int
start "" _ = error "endless noise"
start s n
    | not (hasRepeat (take n s)) = n
    | otherwise = 1 + start (drop 1 s) n

hasRepeat :: (Eq a) => [a] -> Bool
hasRepeat [] = False
hasRepeat (x:xs) = (x `elem` xs) || (hasRepeat xs)
