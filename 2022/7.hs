
import qualified Data.Map
import qualified Data.List


data DirectoryEntry = DirectoryEntry Int [String] deriving (Show)

splitBySpace "" = []
splitBySpace line = s1 : splitBySpace rest
  where s1 = takeWhile (\x -> not (x `elem` " ")) line
        rest = drop 1 $ dropWhile (\x -> not (x `elem` " ")) line

filenameString filename = foldr1 (\y x -> x ++ " " ++ y) filename

isCommand input = head input == "$"
isCd input = isCommand input && input !! 1 == "cd"
isLs input = isCommand input && input !! 1 == "ls"
isFile input = (head . head) input `elem` ['0'..'9']


buildFileSystem :: [[String]] -> [String] -> [(String, DirectoryEntry)]
buildFileSystem [] _ = [] 
buildFileSystem (cmd:rest) filename
  | isCd cmd && last cmd == ".." = buildFileSystem rest (tail filename)
  | isCd cmd && last cmd /= ".." = buildFileSystem rest ((last cmd) : filename)
  | isLs cmd = ((filenameString filename), DirectoryEntry directorySize subDirectories) : buildFileSystem remainingEntries filename
  where entries = takeWhile (not . isCommand) rest
        subDirectories = map (\x -> filenameString ((x !! 1) : filename)) $ filter (not . isFile) entries
        directorySize = foldl (\acc x -> if isFile x then acc + read (head x) else acc) 0 entries
        remainingEntries = dropWhile (not . isCommand) rest


calcSize fs dirName = case Data.Map.lookup dirName fs of
  Just (DirectoryEntry dirSize subDirs) -> dirSize + sum (map (calcSize fs) subDirs)
  Nothing -> 0


getDirnames lines = filter (\x -> isCommand x && length x == 3 && (x !! 2) /= "..") lines
 

-- Part 1
-- main = do
--   cmdOutput <- readFile "7.txt"
--   let rows = lines cmdOutput
--   let cmdLines = map splitBySpace rows
--   let fileSystem = Data.Map.fromList $ buildFileSystem cmdLines []
--   let sizes = map (calcSize fileSystem) (Data.Map.keys fileSystem)
--   let ans = sum $ filter (<= 100000) sizes
--   print sizes
--   print ans 


-- Part 2
main = do
  cmdOutput <- readFile "7.txt"
  let rows = lines cmdOutput
  let cmdLines = map splitBySpace rows
  let fileSystem = Data.Map.fromList $ buildFileSystem cmdLines []
  let sizes = map (calcSize fileSystem) (Data.Map.keys fileSystem)
  -- print $ length sizes
  -- print fileSystem
  print $ Data.List.sort sizes
  let currentlyFree = 70000000 - maximum sizes
  let deleteCandidates = Data.List.sort $ filter (\x -> currentlyFree + x >= 30000000) sizes
  let ans = head deleteCandidates
  print deleteCandidates
  print ans
  
