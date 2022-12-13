-- Day 13: Oh no --

{-
camp5 coding weeeee
Lists of either int or... list of int... or list of list of int... or...
What is a Haskeller to do
Even using an Either type is infeasible
-}

import Data.Char

munchNum :: String -> (Int,String)
munchNum s = let (a,b) = span isDigit s in (read a, b)

listifyInitNum :: String -> String
listifyInitNum s = case span isDigit s of
  ([]    , _ )   -> error "no initial digits found; listification failed"
  (digits, rest) -> "["++digits++"]"++rest


cmp :: String -> String -> Bool -- True means they are in order
cmp [] [] = error "ran out of Left and Right string w no decision"
cmp left@(l:ls) right@(r:rs)
  = case (l,r) of
      (',',',') -> cmp ls rs
      ('[','[') -> cmp ls rs
      (']',']') -> cmp ls rs
      (']', _ ) -> True -- Left ran out first
      ( _ ,']') -> False -- Right ran out first
      ('[', d ) -> if (not (isDigit d))
                   then error ((show d)++" is not digit R")
                   else cmp left (listifyInitNum right)
      ( d ,'[') -> if (not (isDigit d))
                   then error ((show d)++" is not digit L")
                   else cmp (listifyInitNum left) right
      ( ld, rd) -> if (not (isDigit ld)) || (not (isDigit rd))
                   then error ((show ld)++" or "++(show rd)++" not digit")
                   else let (ln, lrest) = munchNum left
                            (rn, rrest) = munchNum right
                          in if ln==rn then cmp lrest rrest
                          else ln < rn


readInputBlanks :: FilePath -> IO [[String]]
readInputBlanks fp = do input <- readFile fp
                        return (splitOnStr "" (lines input))

splitOnStr :: String -> [String] -> [[String]]
splitOnStr _    []  = []
splitOnStr word xs  = let (l, xs') = break (== word) xs
                        in l : case xs' of
                              []        -> []
                              (_:xs'')  -> splitOnStr word xs''

yeetsort :: [String] -> [String]
yeetsort []            =  []
yeetsort (x:xs)        =  yeetsort [y | y <- xs, cmp y x ]
                        ++ [x]
                        ++ yeetsort [y | y <- xs, not (cmp y x)]


main :: IO ()
main
  = do input <- readInputBlanks "input.txt"
       let results = [cmp (head l) (head (drop 1 l)) | l <- input]
           idxdResults = zip [1..(length results)] results
           trueResults = filter (\x -> snd x) idxdResults
           part1answer = sum (map fst trueResults)
           -- Part 2
           flatPlusDiv = concat input ++ ["[[2]]","[[6]]"]
           sortedFlat = yeetsort flatPlusDiv
           idxdSorted = zip [1..(length sortedFlat)] sortedFlat
           idxFstDiv = fst (head (filter (\x->snd x=="[[2]]") idxdSorted))
           idxSndDiv = fst (head (filter (\x->snd x=="[[6]]") idxdSorted))
           part2answer = idxFstDiv*idxSndDiv
         in do putStrLn ("Part One  6101: " ++ (show part1answer))
               putStrLn ("Part Two 21909: " ++ (show part2answer))
