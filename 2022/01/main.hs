-- I forgot how to Haskell.

-- Read input, splitting on newlines
readInput :: (Read a) => FilePath -> IO [a]
readInput fp = do input <- readFile fp
                  return (map read (lines input))

-- When input has blank lines: Keep it dumb
readMaybeInput :: (Read a) => FilePath -> IO [Maybe a]
readMaybeInput fp = do input <- readFile fp
                       return (map read' (lines input))
                    where read' :: (Read a) => String -> Maybe a
                          read' "" = Nothing
                          read' s = Just (read s)


-- Have not seen part 2 so keep it general:

-- Awfully inefficient. Could just build backwards but NO what if part 2
splitOnNothing :: [Maybe a] -> [[a]]
splitOnNothing maybes = split' maybes [] []
    where split' [] outAcc inAcc = outAcc ++ [inAcc]
          split' (Nothing:ms) outAcc inAcc = split' ms (outAcc ++ [inAcc]) []
          split' (Just v:ms) outAcc inAcc = split' ms outAcc (inAcc ++ [v])

-- For Part 1
highestSum :: [[Int]] -> Int
highestSum xss = maximum (map sum xss)

-- For Part 2 - Why did I pick Haskell again
sums :: [[Int]] -> [Int]
sums xss = map sum xss

-- ¯\_(ツ)_/¯
insertionSort :: [Int] -> [Int]
insertionSort xs = insertionSort' xs []
    where insertionSort' [] as = as
          insertionSort' (x:xs) as = insertionSort' xs (insert x as)
              where insert x [] = [x]
                    insert x (a:as) = if x < a then x:a:as else a:(insert x as)
-- Postmortem note:
-- ^This is really very bad--the nice thing about insertion sort is that it's
-- fast in the already-sorted case, so when you are writing in Haskell and
-- you want to be picking things off the _head_ of a list, don't go and pick
-- insertion sort tout bêtement.
-- In this case the input is unlikely to be already-sorted anyway.
-- (Actually the correct thing to do here would have been to sort in decreasing
-- order, thereby fixing this issue _and_ removing the need for the `reverse`
-- later on, but all that's particular to this specific problem.)


main :: IO ()
main = do maybeCalories <- readMaybeInput "input.txt"
          let elfDelimitedCalories = splitOnNothing maybeCalories
              partOne = highestSum elfDelimitedCalories
              sortedCalorieSums = insertionSort (sums elfDelimitedCalories)
              partTwo = sum (take 3 (reverse sortedCalorieSums))
            in do putStrLn ("Part One: " ++ (show partOne))
                  putStrLn ("Part Two: " ++ (show partTwo))

-- For this input should get:
-- Part One: 64929
-- Part Two: 193697
