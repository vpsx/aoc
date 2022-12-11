-- Day 11 wow --

import Data.Array -- wee woo finally! / but no deque

main :: IO ()
main
  = do --
       let rounds = iterate playOneRound (mkyInspects, mkyItems) --forever
           (after20rdsInspects, _) = head (drop 20 rounds)
           (after10000rdsInspects, _) = head (drop 10000 rounds)
         in do putStrLn "Part One: 334*332 = 110888"
               putStrLn "Part Two: 159983*159957 = 25590400731"
               putStrLn (show after20rdsInspects)
               putStrLn (show after10000rdsInspects)

data Monkey = Mky { op :: Int -> Int, test :: Int -> Int }

mkyItems = array (0,7)
  [ (0, [54, 89, 94]),
    (1, [66, 71]),
    (2, [76, 55, 80, 55, 55, 96, 78]),
    (3, [93, 69, 76, 66, 89, 54, 59, 94]),
    (4, [80, 54, 58, 75, 99]),
    (5, [69, 70, 85, 83]),
    (6, [89]),
    (7, [62, 80, 58, 57, 93, 56])
  ]

mkyInspects = array (0,7) [ (i, 0) | i <- [0..7] ]

monkeys = array (0,7) [ (0, m0), (1, m1), (2, m2),
                        (3, m3), (4, m4), (5, m5),
                        (6, m6), (7, m7)           ]

m0 = Mky (\ x -> x * 7)
         (\ x -> if rem x 17 == 0 then 5 else 3)
m1 = Mky (\ x -> x + 4)
         (\ x -> if rem x 3 == 0 then 0 else 3)
m2 = Mky (\ x -> x + 2)
         (\ x -> if rem x 5 == 0 then 7 else 4)
m3 = Mky (\ x -> x + 7)
         (\ x -> if rem x 7 == 0 then 5 else 2)
m4 = Mky (\ x -> x * 17)
         (\ x -> if rem x 11 == 0 then 1 else 6)
m5 = Mky (\ x -> x + 8)
         (\ x -> if rem x 19 == 0 then 2 else 7)
m6 = Mky (\ x -> x + 6)
         (\ x -> if rem x 2 == 0 then 0 else 1)
m7 = Mky (\ x -> x * x)
         (\ x -> if rem x 13 == 0 then 6 else 4)

monkeyLCM = 17*3*5*7*11*19*2*13 -- part 2 :O


playOneRound (insps, itms) = foldl monkeyMove (insps, itms) [0..7]


monkeyMove :: (Array Int Int, Array Int [Int])
              -> Int
              -> (Array Int Int, Array Int [Int])
monkeyMove (mInspects,mItems) mkyNbr
  = let items = mItems!mkyNbr
        newInspCount = (mInspects!mkyNbr) + length items
        itemsWorrified = map (op (monkeys!mkyNbr)) items
        --itemsBored = map (\x -> quot x 3) itemsWorrified --part1
        itemsBored = map (\x -> mod x monkeyLCM) itemsWorrified --part2
        t = test (monkeys!mkyNbr)
        -- throws is like: [(throw-this, to-this-monkey)]
        throws = [(i, t i) | i <- itemsBored]
      in (mInspects // [(mkyNbr, newInspCount)], throwItems (mItems // [(mkyNbr, [])]) throws)


throwItems :: Array Int [Int] -> [(Int, Int)] -> Array Int [Int]
throwItems a [] = a
throwItems a ((item, dstMky):throws)
  = let dstMkyBefore = a!dstMky
        dstMkyAfter = dstMkyBefore ++ [item]
      in throwItems (a // [(dstMky, dstMkyAfter)]) throws
