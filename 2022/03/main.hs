-- Puzzle guarantees that Naughty Packer Elf messed up exactly one item per
-- rucksack. So I will not account for perfect rucksacks.
-- Nor check for empty rucksacks or odd rucksacks...

sndHalf :: String -> String
sndHalf [] = error "empty string given"
sndHalf s = snd' s s where
    snd' s [] = s
    snd' a b = snd' (drop 1 a) (drop 2 b)

findBadItem :: String -> Char
findBadItem s = findBad' s (sndHalf s) where
    findBad' (c:cs) snd = if c `elem` snd then c else findBad' cs snd

priority :: Char -> Int
priority c = pr' c (zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 .. 52]) where
    pr' c [] = error "elfin misbehavior suspected"
    pr' c ((fst, snd):az) = if c == fst then snd else pr' c az


-- Part 2

findBadgeItem :: String -> String -> String -> Char
findBadgeItem (a:as) bs cs = if a `elem` bs && a `elem` cs
                           then a
                           else findBadgeItem as bs cs
findBadgeItem _ _ _ = error "unbadged elf group"

badgesInThrees :: [String] -> [Char]
badgesInThrees [] = []
badgesInThrees (a:b:c:rest) = (findBadgeItem a b c):(badgesInThrees rest)
badgesInThrees _ = error "odd elves out"

-- Read input line-by-line as strings
readInput :: FilePath -> IO [String]
readInput fp = do input <- readFile fp
                  return (lines input)


main :: IO ()
main = do rucksacks <- readInput "input.txt"
          let badItems = map findBadItem rucksacks
              part1answer = sum (map priority badItems)
              --part 2
              badges = badgesInThrees rucksacks
              part2answer = sum (map priority badges)
            in do putStrLn ("Part One: " ++ (show part1answer)) --8243
                  putStrLn ("Part Two: " ++ (show part2answer)) --2631
