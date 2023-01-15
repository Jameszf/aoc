
import Data.Char


separateColumns _ 0 = []
separateColumns stack count = col : separateColumns rest (count - 1)
  where col = map (take 3) stack
        rest = map (drop 4) stack


extractNumbers :: [Char] -> [Int]
extractNumbers "" = []
extractNumbers op = read num : extractNumbers rest
  where reachNum = dropWhile (\x -> not (isDigit x)) op
        num = takeWhile (\x -> isDigit x) reachNum
        rest = dropWhile (\x -> isDigit x) reachNum


-- Part 1
-- swap n c1 c2 = (newC1, newC2)
--   where newC1 = drop n c1
--         newC2 = (reverse $ take n c1) ++ c2
  

-- Part 2
swap n c1 c2 = (newC1, newC2)
  where newC1 = drop n c1
        newC2 = (take n c1) ++ c2


sim columns [] = columns
sim columns (op:restOps) = sim newColumns restOps
  where newColumns = map (\x -> if snd x == (op !! 1)
                           then newC1
                           else if snd x == (op !! 2)
                           then newC2
                           else fst x) (zip columns [1..])
        (newC1, newC2) = swap (op !! 0) (columns !! ((op !! 1) - 1)) (columns !! ((op !! 2) - 1))


main = do
  fileContents <- readFile "5.txt"
  let rows = lines fileContents
  let stack = init $ takeWhile (\x -> x /= "") rows
  let operations = map extractNumbers (drop ((length stack) + 2) rows)
  -- print $ swap 3 ["a", "b", "c", "d"] ["e"]
  let columns = map (dropWhile (\x -> x == "   ")) (separateColumns stack 9)
  print $ sim columns operations
  
  let ans = foldr1 (\x acc -> head x : acc) (sim columns operations)
  print ans
      
      
