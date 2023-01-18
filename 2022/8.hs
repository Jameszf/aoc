
between list = (init . tail) list


makeColumns rows |
  (length $ head rows) == 0 = []
makeColumns rows = column : makeColumns rest
  where column = map (head) rows
        rest = map (drop 1) rows


makeIntList :: String -> [Int]
makeIntList [] = []
makeIntList (fstChar:rest) = read [fstChar] : makeIntList rest
  

countVisibleTrees :: Int -> [Int] -> Int
countVisibleTrees _ [] = 0
countVisibleTrees tallest (tree:rest) 
  | tree > tallest = 1 + (countVisibleTrees tree rest)
  | otherwise = countVisibleTrees tallest rest
  

main = do
  input <- readFile "8.txt"
  let rows = map makeIntList (lines input)
  let columns = makeColumns rows
  let perimeter = 2 * length (head rows) + 2 * length (head columns) - 4
  let north = sum $ map (countVisibleTrees (-1)) (between columns)
  let south = sum $ map (countVisibleTrees (-1)) (map reverse (between columns))
  let east = sum $ map (countVisibleTrees (-1)) (map reverse (between rows))
  let west = sum $ map (countVisibleTrees (-1)) (between rows)

  let ans = north + south + east + west + perimeter
  print ans
    
  
