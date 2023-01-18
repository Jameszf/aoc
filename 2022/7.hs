
import qualified Data.Map


data DirectoryEntry = DirectoryEntry Int [String] deriving (Show)

splitBySpace "" = []
splitBySpace line = s1 : splitBySpace rest
  where s1 = takeWhile (\x -> not (x `elem` " ")) line
        rest = drop 1 $ dropWhile (\x -> not (x `elem` " ")) line


isCommand input = input !! 0 == "$"
isFile input = (input !! 0) !! 0 `elem` ['0'..'9']

folderOp cmd1 cmd2 = isCommand cmd1 &&
                     isCommand cmd2 &&
                     cmd1 !! 1 == "cd" &&
                     cmd2 !! 1 == "ls"


-- Assuming input contains no two consecutive cds into folders
-- e.g. $ cd folder_name_1
--      $ cd folder_name_2
buildFileSystem [] = [] 
buildFileSystem (cmd1:cmd2:rest) = if folderOp cmd1 cmd2
  then (cmd1 !! 2, DirectoryEntry directorySize subDirectories) : buildFileSystem remainingEntries
  else buildFileSystem (cmd2:rest)
  where entries = takeWhile (not . isCommand) rest
        subDirectories = map (!! 1) $ filter (not . isFile) entries
        directorySize = foldl (\acc x -> if isFile x then acc + read (x !! 0) else acc) 0 entries
        remainingEntries = dropWhile (not . isCommand) rest


calcSize fs dirName = case Data.Map.lookup dirName fs of
  Just (DirectoryEntry dirSize subDirs) -> dirSize + sum (map (calcSize fs) subDirs)
  Nothing -> 0


getDirnames lines = filter (\x -> isCommand x && length x == 3 && (x !! 2) /= "..") lines
 

main = do
  cmdOutput <- readFile "7.txt"
  let rows = lines cmdOutput
  let cmdLines = map splitBySpace rows
  -- print cmdLines
  let fileSystem = Data.Map.fromList $ buildFileSystem cmdLines
  let sizes = map (calcSize fileSystem) (Data.Map.keys fileSystem)
  let ans = sum $ filter (<= 100000) sizes
  -- print fileSystem
  -- print $ Data.Map.size fileSystem
  -- print sizes
  -- print ans 
  print $ getDirnames cmdLines
  
