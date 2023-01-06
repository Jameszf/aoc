
import Data.List


findSeparator :: [[Char]] -> Int -> Int
findSeparator [] count = count + 1
findSeparator (x:xs) count = if x == ""
  then count
  else findSeparator xs (count + 1)

separateElves :: [[Char]] -> [[[Char]]] -> [[[Char]]]
separateElves [] processed = processed
separateElves calories processed = separateElves newList (subList:processed)
  where subList = take ((findSeparator calories 0)) calories
        newList = drop ((findSeparator calories 0) + 1) calories

main = do
     fileContents <- readFile "1.txt"
     let separated = [[read x | x <- xs] | xs <- separateElves (lines fileContents) []]
     let sums = (reverse . sort) [sum x | x <- separated]
     print ("1: " ++ ((show . head) sums))
     print ("2: " ++ ((show . sum . (take 3)) sums))

     -- printList calories
     putStrLn "Program Finished."
