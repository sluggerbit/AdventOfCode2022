import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import qualified GHC.Data.FiniteMap as M

type Priorities = Map.Map Item Prio

type Item = Char
type Prio = Int

-- pre-processing
smallLetters :: String
smallLetters = "abcdefghijklmnopqrstuvwxyz"

bigLetters :: String
bigLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

prio1 :: [Int]
prio1 = [1 .. 26]

prio2 :: [Int]
prio2 = [27 .. 52]

priorities :: Priorities
priorities = M.insertList (zip smallLetters prio1) $ Map.fromList $ zip bigLetters prio2

-- main
main :: IO ()
main = do
    input <- readFile "input.txt"
    let strings = words input
        p =  priorities
    print $ sum $ findDupl strings p []
    print $ sum $ findBadges strings p []

-- first task
findDupl :: [String] -> Priorities ->  [Int] -> [Int]
findDupl [] _ dupl = dupl
findDupl (s:ss) m dupl = do
    let comp1 = take (length s `div` 2) s
        comp2 = drop (length comp1) s
    case Map.lookup (compareLs s comp1 comp2) m of
     Just int -> findDupl ss m (int:dupl)
     Nothing  -> [0]
-- first task
compareLs :: [Char] -> String -> String -> Char
compareLs (d:ds) c1 c2
    | d `elem` c1 && d `elem` c2 = d
    | otherwise = compareLs ds c1 c2


-- second task
findBadges :: [String] -> Priorities -> [Int] -> [Int]
findBadges [] _ ints = ints
findBadges (x:y:z:ss) m ints = do
    let d = duplicates "" "" $ duplicates "" "" $ concatMap nub [x, y, z]
    findBadges ss m (head (lookupType [] m d) :ints)
-- second task
lookupType :: [Int] -> Priorities -> String -> [Int]
lookupType out m []     = out
lookupType out m (c:cs) = case Map.lookup c m of
        Just int -> lookupType (int:out) m cs
        Nothing -> [0]
-- second task
duplicates :: String -> String -> String -> String
duplicates out visited [] = out
duplicates out visited (s:ss)  
    | s `elem` visited = duplicates out visited ss
    | s `elem` ss      = duplicates (s:out) visited ss
    | otherwise        = duplicates out visited ss 

{-
--- Day 3: Rucksack Reorganization ---
Each rucksack has two large compartments. 

ex:
vJrwpWtwJgWrhcsFMMfFFhFp
contains items vJrwpWtwJgWrhcsFMMfFFhFp, 
first compartment: vJrwpWtwJgWr
second compartment: hcsFMMfFFhFp. p appears in both comp.
prio for p = 16

a - z prio 1 - 26.
A - Z prio 27 - 52

item type that appears in both compartments, sum of the prio? 

--- Part Two ---
Groups of three, carries badges
if a group's badge is item type B, all three members will have item type B 
    at most two members will be carrying any other item type.

Correct badge item type = common between all group member

Three lines in your list corresponds to a single group

Find sum of Prio for all badges.
-}