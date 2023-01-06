
-- First part
-- choiceScore x = snd $ head $ filter ((x ==) . fst) [('X', 1), ('Y', 2), ('Z', 3)]

-- pairOutcome x y = head $ filter pred poss
--   where pred = \(a, b, _) -> a == x && b == y
--         poss = [('A', 'X', 3), ('A', 'Y', 6), ('A', 'Z', 0),
--                  ('B', 'X', 0), ('B', 'Y', 3), ('B', 'Z', 6),
--                  ('C', 'X', 6), ('C', 'Y', 0), ('C', 'Z', 3)]


-- main = do
--   fileContents <- readFile "2.txt" 
--   let pairings = lines fileContents
--   let func = \x -> pairOutcome (x !! 0) (x !! 2)
--   let scores = map func pairings
--   let ans = foldl (\acc (_, c, x) -> acc + x + (choiceScore c)) 0 scores
--   print ans


-- Second Part
choiceScore x = (snd . head) $ filter ((x ==) . fst) [('X', 0), ('Y', 3), ('Z', 6)]

pairOutcome x y = head $ filter pred poss
  where pred = \(a, b, _) -> a == x && b == y
        poss = [('A', 'X', 3), ('A', 'Y', 1), ('A', 'Z', 2),
                 ('B', 'X', 1), ('B', 'Y', 2), ('B', 'Z', 3),
                 ('C', 'X', 2), ('C', 'Y', 3), ('C', 'Z', 1)]

main = do
  fileContents <- readFile "2.txt" 
  let pairings = lines fileContents
  let func = \x -> pairOutcome (x !! 0) (x !! 2)
  let scores = map func pairings
  let ans = foldl (\acc (_, c, x) -> acc + x + (choiceScore c)) 0 scores
  print ans
  
