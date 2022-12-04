main :: IO ()
main = do
    input <- readFile "input.txt"
    let x =   words input
    --print $ containsPair (head x)
    print $ numberOfContainedPairs (0, 0) x
    return ()

numberOfContainedPairs :: (Int, Int) -> [String] -> (Int, Int)
numberOfContainedPairs (i1, i2) [] = (i1, i2)
numberOfContainedPairs (i1, i2) (s:ss) =  numberOfContainedPairs ((containsPair s + i1), (overlap s + i2)) ss

containsPair :: String -> Int
containsPair s = do
    let (ls1, ls2) = doLsFromString s
        b1 = all (\x -> x `elem` ls2) ls1
        b2 = all (\x -> x `elem` ls1) ls2
    if (b1 || b2) 
    then 1 
    else 0

overlap :: String -> Int
overlap s  = do
    let (ls1, ls2) = doLsFromString s
        b1 = any (\x -> x `elem` ls2) ls1
        b2 = any (\x -> x `elem` ls1) ls2
    if (b1 || b2) 
    then 1 
    else 0

doLsFromString :: String -> ([Int], [Int])
doLsFromString s = do
    let (ss1, ss2) = getCompartments ',' s
        (h1, t1) = getCompartments '-' ss1
        (h2, t2) = getCompartments '-' ss2
    ([(read h1 :: Int) ..  (read t1 :: Int)], [(read h2 :: Int) ..  (read t2 :: Int)])

getCompartments ::  Char -> String ->  (String, String)
getCompartments c s = do
    let h = takeWhile (/= c) s
        t = drop (length h +1) s
    (h, t)
{-
--- Day 4: Camp Cleanup ---
section -> unique ID number
each Elf is assigned a range of section IDs.

Assignments overlap. 

In the example there are 2 fully contained within the other pairs.

In how many assignment pairs does one range fully contain the other?

--- Part Two ---
It seems like there is still quite a bit of duplicate work planned. 
Instead, the Elves would like to know the number of pairs that overlap at all.


-}