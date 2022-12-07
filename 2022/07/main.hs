-- Day 7 /\/\/\

readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)

main :: IO ()
main
  = do outputLns <- readInputStrs "input.txt"
       let filesys = build (Dir "/" []) (outputLns, [])
           part1answer = sumDirSizesBelow 100000 filesys
           -- part 2
           totalDiskSpace = 70000000
           totalNeedSpace = 30000000
           totalUsedSpace = trSize filesys
           totalUnusedSpace = totalDiskSpace - totalUsedSpace
           stillNeedSpace = totalNeedSpace - totalUnusedSpace
           dirSizesIncr = quicksort (toDirSizeList filesys)
           part2answer = head (dropWhile (<stillNeedSpace) dirSizesIncr)
         in do putStrLn ("Part One 1307902: " ++ (show part1answer))
               putStrLn ("Part Two 7068748: " ++ (show part2answer))


-- No I will not have a dir size field

data Tree = File String Int | Dir String [Tree] deriving Eq

type Path = [String] -- /a/b/c = ["c", "b", "a"]

type Output = [String]

-- Making basically all the assumptions:
-- no `$ cd multi/level/path`,
-- no files with same name but diff size,
-- will not cd into not-yet-ls'd dirs...

build :: Tree -> (Output, Path) -> Tree
build t ([], _) = t
build t ((o:os), path)
    = case (words o) of
        ("$":"ls":_)      -> build t (os, path)
        ("$":"cd":"/":_)  -> build t (os, [])
        ("$":"cd":"..":_) -> build t (os, drop 1 path)
        ("$":"cd":p:_)    -> build t (os, p:path)
        ("dir":dname:_)   -> build (insert t (Dir dname []) path)
                                   (os, path)
        (sz:fname:_)      -> build (insert t (File fname (read sz)) path)
                                   (os, path)


insert :: Tree -> Tree -> Path -> Tree
insert rcving rcvd fp = ins' rcving rcvd (reverse fp) where
    ins' (File _ _) _ _ = error "attempted insert into file"
    ins' (Dir nm conts) rcvd [] = Dir nm (dirInsert conts rcvd)
    ins' (Dir nm conts) rcvd (p:ps)
        = let (a,b) = break (isDirNamed p) conts
          in case b of
               (d:ds) -> Dir nm (a ++ [ins' d rcvd ps] ++ ds)
               [] -> error ("path not found: " ++ p ++ (show ps))
-- Garbage collector needs a raise


isDirNamed :: String -> Tree -> Bool
isDirNamed _ (File _ _) = False
isDirNamed s (Dir dname _) = (dname == s)

-- If dir/file being inserted already exists, do nothing
dirInsert :: [Tree] -> Tree -> [Tree]
dirInsert [] rcvd = [rcvd]
dirInsert conts@(c:cs) rcvd
    = if c == rcvd then conts else c:(dirInsert cs rcvd)


trSize :: Tree -> Int
trSize (File _ sz) = sz
trSize (Dir _ cs) = sum (map trSize cs)


sumDirSizesBelow :: Int -> Tree -> Int
sumDirSizesBelow n (File _ sz) = 0
sumDirSizesBelow n d@(Dir _ conts)
    = let sz = trSize d
       in (if sz <= n then sz else 0) + sum (map (sumDirSizesBelow n) conts)


toDirSizeList :: Tree -> [Int]
toDirSizeList (File _ _) = []
toDirSizeList d@(Dir _ cs) = (trSize d):(concatMap toDirSizeList cs)


quicksort :: (Ord a) => [a] -> [a]
quicksort []            =  []
quicksort (x:xs)        =  quicksort [y | y <- xs, y<x ]
                        ++ [x]
                        ++ quicksort [y | y <- xs, y>=x]
