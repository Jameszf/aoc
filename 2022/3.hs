
import Data.Char


computeScore common = if elem common ['A'..'Z']
  then ord common - ord 'A' + 27
  else ord common - ord 'a' + 1
  
-- Part 1
-- findCommonItem sack = head $ filter (\x -> elem x s2) s1
--   where s1 = take (div (length sack) 2) sack
--         s2 = drop (div (length sack) 2) sack

-- main = do
--   fileContents <- readFile "3.txt" 
--   let sacks = lines fileContents
--   let commons = map findCommonItem sacks
--   let ans = sum $ map computeScore commons
--   print commons
--   print ans

-- Part 2
findCommonItem [] = []
findCommonItem (s1:s2:s3:xs) = (head $ filter (\x -> and [(elem x s2), (elem x s3)]) s1) : findCommonItem xs

main = do
  fileContents <- readFile "3.txt" 
  let sacks = lines fileContents
  let ans = sum $ ((map computeScore) . findCommonItem) sacks
  print ans
  
