import Text.ParserCombinators.Parsec
import System.Environment
import Data.List

data Transition = Transition {
    source :: String,
    dest :: String,
    symbol :: String
} deriving (Eq, Show)

data Machine = Machine {
        states :: [String],
        alphabet :: [String],
        initstate :: String,
        finalstates :: [String],
        transitions :: [Transition]
} deriving (Eq, Show)

------------------------------------------ PARSER ------------------------------------------

file :: GenParser Char st [[String]]
file = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> [[String]]
parseCSV input = case (parse file "(unknown)" input) of
    Left err -> error $ show err
    Right xs -> return xs!!0

--------------------------------------- MACHINE INIT ---------------------------------------
initTransitions :: [[String]] -> [Transition]
initTransitions [] = []
initTransitions (x:xs) = Transition { source = x!!0, dest = x!!2, symbol = x!!1 }:initTransitions xs


initMachine :: [[String]] -> Machine
initMachine xs = Machine {
    states = xs !! 0,
    alphabet = xs !! 1,
    initstate = xs !! 2 !! 0,
    finalstates = xs !! 3,
    transitions = initTransitions $ drop 4 xs
}

--------------------------------------- LOGIC? ---------------------------------------
eliminateUnreachableStates :: Machine -> Machine
eliminateUnreachableStates Machine {states = s, alphabet = a, initstate = i, finalstates = f, transitions = t} =
    Machine [(elim [[i]] [] t)] a i f t -- todo final states
    where 
        elim cur prev transitions 
            | cur == prev = cur
            | otherwise = elim (nub $ addStates cur transitions) cur transitions
            where 
                addStates state (transition:xs) 
                    | state == source transition = [(source transition)]:addStates state xs
                    | otherwise = [] ++ addStates state xs

main :: IO()
main = do
    args <- getArgs
    case args of

        [ "-i", path ] -> do
            csv <- readFile path
            print $ initMachine $ parseCSV csv

            -- let fsm = initMachine $ parseCSV csv
            -- print $ fsm 
            -- print $ eliminateUnreachableStates fsm

        [ "-i" ] -> do
            csv <- getContents
            print $ initMachine $ parseCSV csv

        [ "-t", path ] -> do
            csv <- readFile path
            print $ eliminateUnreachableStates $ initMachine $ parseCSV csv

        [ "-t" ] -> do
            csv <- getContents
            print $ initMachine $ parseCSV csv
        

        _ -> print "Usage: dka-2-mka -i|-t [file_path]"
    
