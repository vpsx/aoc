-- Day 2: Rock Paper Scissors

-- Read input line-by-line as strings
readInput :: FilePath -> IO [String]
readInput fp = do input <- readFile fp
                  return (lines input)


-- Part 1

-- A X rock, B Y paper, C Z scissors
-- 0 for loss, 3 for draw, 6 for win
-- and you are XYZ
outcome :: String -> Int
outcome x = case x of
              "A X" -> 3
              "A Y" -> 6
              "A Z" -> 0
              "B X" -> 0
              "B Y" -> 3
              "B Z" -> 6
              "C X" -> 6
              "C Y" -> 0
              "C Z" -> 3
              _     -> error "invalid round"


-- 1 for rock, 2 for paper, 3 for scissors
shapeScore :: Char -> Int
shapeScore x = case x of
                 'X' -> 1
                 'Y' -> 2
                 'Z' -> 3
                 _     -> error "invalid shape"


-- Part 2

-- X now encodes loss 0, Y draw 3, and Z win 6.
newOutcomeScore :: Char -> Int
newOutcomeScore x = case x of
                 'X' -> 0
                 'Y' -> 3
                 'Z' -> 6
                 _     -> error "invalid end"

-- A rock, B paper, C scissors
-- X now encodes loss 0, Y draw 3, and Z win 6.
-- 1 for rock, 2 for paper, 3 for scissors
newShapeScore :: String -> Int
newShapeScore x = case x of
              "A X" -> 3
              "A Y" -> 1
              "A Z" -> 2
              "B X" -> 1
              "B Y" -> 2
              "B Z" -> 3
              "C X" -> 2
              "C Y" -> 3
              "C Z" -> 1
              _     -> error "invalid round"



main :: IO ()
main = do rounds <- readInput "input.txt"
          let outcomeSum = sum (map outcome rounds)
              myShapes = [last x | x <- rounds]
              shapeSum = sum (map shapeScore myShapes)
              totalScore = outcomeSum + shapeSum
              --part 2
              newOutcomes = myShapes
              newOutcomeSum = sum (map newOutcomeScore newOutcomes)
              newShapeSum = sum (map newShapeScore rounds)
              newTotalScore = newShapeSum + newOutcomeSum
            in do putStrLn ("Part One: " ++ (show totalScore)) -- 9651
                  putStrLn ("Part Two: " ++ (show newTotalScore)) --10560
