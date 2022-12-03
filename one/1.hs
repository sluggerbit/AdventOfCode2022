{-# LANGUAGE FlexibleContexts #-}
import Numeric

main :: IO ()
main = do
    x <- readFile "input.txt"
    let y   = map (count 0 . words) (foldr build [[]] x)
        y'  = removeOne y (maximum' y)
        y'' = removeOne y' (maximum' y')
        f   = maximum' y: maximum' y': maximum' y'':[]
    let result = sum f
    print (maximum' y, result)



removeOne :: [Int] -> Int -> [Int]
removeOne list v =
    case list of
        [] -> error "Element not found!"
        x:xs | v==x -> xs
        x:xs -> x:removeOne xs v

--main = print (remove_one [1,2,3,4] 3)

maximum' :: Ord Int => [Int] -> Int
maximum' [x]       = x
maximum' (x:x':xs) = maximum' ((if x >= x' then x else x'):xs)

build :: Char -> [String] -> [String]
build c (s:ls)
    | null s || head s /= '\n' || c /= '\n' = (c:s)       : ls
    | otherwise                              = [] : tail s : ls

count :: Int -> [String] -> Int
count = foldl (\ int s -> (read s :: Int) + int)
    