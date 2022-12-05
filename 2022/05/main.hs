-- Day 5

main :: IO ()
main
  = do procedures <- readInputStrs "input_doctored.txt"
       let -- watch me zoom
           s1 = "JZGVTDBN"
           s2 = "FPWDMRS"
           s3 = "ZSRCV"
           s4 = "GHPZJTR"
           s5 = "FQZDNJCT"
           s6 = "MFSGWPVN"
           s7 = "QPBVCG"
           s8 = "NPBZ"
           s9 = "JPW"
           stax = [s1,s2,s3,s4,s5,s6,s7,s8,s9]
           -- Part 1
           instrWords = map words procedures -- [["move", "1", "from"...]]
           triplets
             = [(read (head (drop 1 x)),
                 read (head (drop 3 x)),
                 read (head (drop 5 x))) | x <- instrWords]
           endStateStax = foldl crane stax triplets
           part1answer = [ head x | x <- endStateStax]
           -- Part 2
           endStateStax2 = foldl crane9001 stax triplets
           part2answer = [ head x | x <- endStateStax2]
         in do putStrLn ("Part One GFTNRBZPF: " ++ (show part1answer))
               putStrLn ("Part Two VRQWPDSGP: " ++ (show part2answer))


-- Read input, splitting on newlines (Strings)
readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)


-- Each stack is a... stack

-- This being Haskell I guess I must haul around the whole shipyard


crane :: [[Char]] -> (Int, Int, Int) -> [[Char]]
crane stax (n, src, dst)
    | (n==0) = stax
    | otherwise = crane (putdown dst (pickup src stax)) ((n-1), src, dst)


pickup :: Int -> [[Char]] -> (Char, [[Char]])
pickup src stax = let s:s' = head (drop (src-1) stax)
                    in (s, (take (src-1) stax) ++ [s'] ++ (drop src stax))

putdown :: Int -> (Char, [[Char]]) -> [[Char]]
putdown dst (c, stax) = let s = head (drop (dst-1) stax)
                          in (take (dst-1) stax) ++ [c:s] ++ (drop dst stax)

-- Part 2 CrateMover 9001 weeeeee

crane9001 :: [[Char]] -> (Int, Int, Int) -> [[Char]]
crane9001 stax (n, src, dst) = (putdown9001 dst (pickup9001 n src stax))

pickup9001 :: Int -> Int -> [[Char]] -> ([Char], [[Char]])
pickup9001 n src stax
    = let (pickedup, rest) = splitAt n (head (drop (src-1) stax))
        in (pickedup, (take (src-1) stax) ++ [rest] ++ (drop src stax))

putdown9001 :: Int -> ([Char], [[Char]]) -> [[Char]]
putdown9001 dst (pickedup, stax)
    = let s = head (drop (dst-1) stax)
        in (take (dst-1) stax) ++ [pickedup ++ s] ++ (drop dst stax)
