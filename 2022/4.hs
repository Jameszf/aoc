

separateByDelim line delim = [s1, s2]
  where s1 = takeWhile (\x -> x /= delim) line
        s2 = (drop 1) $ dropWhile (\x -> x /= delim) line


makePairs line = [read n1, read n2, read n3, read n4]
  where (n1:n2:_) = separateByDelim r1 '-'
        (n3:n4:_) = separateByDelim r2 '-'
        (r1:r2:_) = separateByDelim line ','

-- Part 1
-- sol :: [[Int]] -> Int
-- sol [] = 0
-- sol (r1:rs) = if ((n1 <= n3) && (n2 >= n4)) || ((n3 <= n1) && (n4 >= n2))
--   then sol rs + 1
--   else sol rs
--   where (n1:n2:n3:n4:_) = r1


-- main = do
--   fileContents <- readFile "4.txt"
--   let pairs = (map makePairs) $ lines fileContents
--   print $ sol pairs

-- Part 2  
sol :: [[Int]] -> Int
sol [] = 0
sol (r1:rs) = if not (n3 > n2 || n1 > n4)
  then sol rs + 1
  else sol rs
  where (n1:n2:n3:n4:_) = r1


main = do
  fileContents <- readFile "4.txt"
  let pairs = (map makePairs) $ lines fileContents
  print $ sol pairs
