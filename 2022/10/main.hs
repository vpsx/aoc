-- Day 10 --

readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)

main :: IO ()
main
  = do instrs <- readInputStrs "input.txt"
       let -- Part 1
           signal = runcpu instrs
           part1answer = sum [sigStrength (head (drop (x-1) signal))
                             | x <- [20, 60 .. 220]]
           -- Part 2
           pixels = map pix (take (40*6) signal)
           part2answer = screenify pixels
         in do putStrLn ("Part One 15120: " ++ (show part1answer))
               putStrLn ("Part Two RKPJBPLA: ")
               putStrLn part2answer


inc x = x + 1

sigStrength (cycle, regx) = cycle * regx

runcpu instrs = cpu instrs (1, 1, Nothing)

-- [instructions] -> (current cycle, reg x, addx buffer) -> [(cycle,regx)]
cpu :: [String] -> (Int, Int, Maybe Int) -> [(Int, Int)]
cpu instrs (c, regx, Just n)
  = (c, regx):(cpu instrs (inc c, regx+n, Nothing))
cpu instrs (c, regx, Nothing)
  = case instrs of
      [] -> []
      (i:is) -> if i=="noop"
                then (c, regx):(cpu is (inc c, regx, Nothing))
                else let (_:n:_) = words i -- Just assume "addx" :P
                      in (c, regx):(cpu is (inc c, regx, Just (read n)))


pix :: (Int, Int) -> Char
pix (cycle, spritePos)
  = let crtPos = (mod cycle 40)-1
      in if abs (crtPos-spritePos) <= 1 then '#' else '.'

screenify :: [Char] -> [Char]
screenify [] = []
screenify s = (take 40 s)++"\n"++(screenify (drop 40 s))
