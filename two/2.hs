import Numeric

main :: IO ()
main = do
    x <- readFile "input.txt"
    let y = words x
    print (saveToLists y 0, saveToLists' y 0)

saveToLists :: [String] -> Int  -> Int
saveToLists [] score = score
saveToLists (x:y:ss) score = case x of 
    "A" -> case y of
        "X" -> saveToLists ss (score + outcome "draw" + choice "rock")
        "Y" -> saveToLists ss (score + outcome "win"  + choice "paper")
        "Z" -> saveToLists ss (score + outcome "loss" + choice "scissors")
    "B" -> case y of
        "X" -> saveToLists ss (score + outcome "loss" + choice "rock") 
        "Y" -> saveToLists ss (score + outcome "draw" + choice "paper") 
        "Z" -> saveToLists ss (score + outcome "win"  + choice "scissors") 
    "C" -> case y of
        "X" -> saveToLists ss (score + outcome "win"  + choice "rock") 
        "Y" -> saveToLists ss (score + outcome "loss" + choice "paper") 
        "Z" -> saveToLists ss (score + outcome "draw" + choice "scissors") 

saveToLists' :: [String] -> Int -> Int
saveToLists' [] score = score
saveToLists' (x:y:ss) score = case x of 
    "A" -> case y of -- Rock
        "X" -> saveToLists' ss (score + (outcome "loss" + choice "scissors")) 
        "Y" -> saveToLists' ss (score + (outcome "draw" + choice "rock"))     
        "Z" -> saveToLists' ss (score + (outcome "win"  + choice "paper"))    
    "B" -> case y of 
        "X" -> saveToLists' ss (score + (outcome "loss" + choice "rock"))     
        "Y" -> saveToLists' ss (score + (outcome "draw" + choice "paper"))    
        "Z" -> saveToLists' ss (score + (outcome "win"  + choice "scissors")) 
    "C" -> case y of -- 
        "X" -> saveToLists' ss (score + (outcome "loss" + choice "paper"))    
        "Y" -> saveToLists' ss (score + (outcome "draw" + choice "scissors")) 
        "Z" -> saveToLists' ss (score + (outcome "win"  + choice "rock"))     

outcome :: String -> Int 
outcome "win"  = 6
outcome "draw" = 3
outcome "loss" = 0

choice :: String -> Int 
choice "rock"     = 1
choice "paper"    = 2
choice "scissors" = 3

{-
X lose, Y draw, Z win

The total score is still calculated in the same way, 
but now you need to figure out what shape to choose so the round ends as indicated. 

Following the Elf's instructions for the second column, 
what would your total score be if everything goes exactly according to your strategy guide?
-}


{-
--- Day 2: Rock Paper Scissors ---
Part 1
 A Rock, B Paper, C Scissors. 
 X Rock, Y Paper, Z Scissors.

 Scoring:
 total: (Score for shape selected) + (Score for outcome)

 Score for shape selected: 1 Rock, 2 Paper, 3 Scissors
 Score for outcome       : 0 loss, 3 draw , 6 win
 
 What would your total score be if everything goes exactly according to your strategy guide?
    -}
