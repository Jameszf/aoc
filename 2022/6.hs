
import qualified Data.Set


  
nDiff n packet count = if (Data.Set.size $ Data.Set.fromList (take n packet)) == n
  then count
  else nDiff n (drop 1 packet) (count + 1)


main = do
  packet <- readFile "6.txt"
  print $ nDiff 14 packet 14
  print $ nDiff 4 packet 4
  

-- main = do
--   packet <- readFile "6.txt"
--   print $ fourDiff 14 packet 14
