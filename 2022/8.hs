
transpose rows |
  (length $ head rows) == 0 = []
transpose rows = column : transpose rest
  where column = map (head) rows
        rest = map (drop 1) rows


reverseRows grid = map reverse grid


makeIntList :: String -> [Int]
makeIntList [] = []
makeIntList (fstChar:rest) = read [fstChar] : makeIntList rest
  

-- Part 1
-- markVisibleTrees :: Int -> [Int] -> [Int]
-- markVisibleTrees _ [] = []
-- markVisibleTrees tallest (tree:rest) 
--   | tree > tallest = 1 : (markVisibleTrees tree rest)
--   | otherwise = 0 : (markVisibleTrees tallest rest)
  

-- markGrid grid = map (markVisibleTrees (-1)) grid
  

-- combineTwoRows :: [Int] -> [Int] -> [Int]
-- combineTwoRows [] [] = []
-- combineTwoRows (head1:rest1) (head2:rest2)
--   | head1 == 1 || head2 == 1 = 1 : combineTwoRows rest1 rest2
--   | otherwise = 0 : combineTwoRows rest1 rest2
  
-- combineTwoMaps :: [[Int]] -> [[Int]] -> [[Int]]
-- combineTwoMaps [] [] = []
-- combineTwoMaps (row1:map1) (row2:map2) = combineTwoRows row1 row2 : combineTwoMaps map1 map2
  
-- combineMaps :: [[[Int]]] -> [[Int]]
-- combineMaps maps = foldl1 (\acc x -> combineTwoMaps acc x) maps


-- countVisible treemap = sum $ map sum treemap
  

-- main = do
--   input <- readFile "8.txt"
--   let rows = map makeIntList (lines input)
--   let columns = transpose rows
--   let perimeter = 2 * length (head rows) + 2 * length (head columns) - 4
--   -- North, South, East, West
--   let ops = [transpose, reverseRows . transpose, reverseRows, \x -> x]
--   let inverse = [transpose, transpose . reverseRows, reverseRows, \x-> x]

--   let visibleTreemaps = map (\op -> markGrid $ op rows) ops
--   let orientatedTreemaps = map (\x -> (fst x) (snd x)) (zip inverse visibleTreemaps)

--   let ans = countVisible $ combineMaps orientatedTreemaps
--   print ans
  
-- Part 2
countVisibleTrees [] _ = 0
countVisibleTrees (tree:rest) spot
  | spot > tree = 1 + countVisibleTrees rest spot
  | spot <= tree = 1

calcRowScores [] = []
calcRowScores (tree:rest) = countVisibleTrees rest tree : calcRowScores rest

calcOneDirScores treemap = map calcRowScores treemap


combineTwoRows :: [Int] -> [Int] -> [Int]
combineTwoRows [] [] = []
combineTwoRows (head1:rest1) (head2:rest2) = (head1 * head2) : combineTwoRows rest1 rest2
  
combineTwoMaps :: [[Int]] -> [[Int]] -> [[Int]]
combineTwoMaps [] [] = []
combineTwoMaps (row1:map1) (row2:map2) = combineTwoRows row1 row2 : combineTwoMaps map1 map2
  
combineMaps :: [[[Int]]] -> [[Int]]
combineMaps maps = foldl1 (\acc x -> combineTwoMaps acc x) maps
  
  
main = do
  input <- readFile "8.txt"
  let rows = map makeIntList (lines input)

  let ops = [transpose, reverseRows . transpose, reverseRows, \x -> x]
  let inverse = [transpose, transpose . reverseRows, reverseRows, \x-> x]
  let scoreTreemaps = map (\op -> calcOneDirScores $ op rows) ops
  let orientatedTreemaps = map (\x -> (fst x) (snd x)) (zip inverse scoreTreemaps)

  let ans = maximum $ map maximum $ combineMaps orientatedTreemaps
  print ans
