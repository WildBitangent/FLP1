import Text.ParserCombinators.Parsec
import System.Environment

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

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse file "(unknown)" input

getCSVString :: Either ParseError [[String]] -> [[String]]
getCSVString input = case input of
    Left err -> error "Failed to parse input!"
    Right xs -> return xs    


-- parseCSV :: String -> [[String]]
-- parseCSV input = do
--     let parsed = parse file "(unknown)" input  
--     case (parsed) of
--         Left err -> error "Failed to parse input!"
--         Right xs -> return xs


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
    Machine (elim [i] [] t) a i f t -- todo final states
    where
        elim cur prev transitions = cur
            -- | cur == prev = cur
            -- | otherwise = [] ++ elim (addStates cur transitions) cur transitions
            -- where addStates (x:xs) (y:ys) = 

main :: IO()
main = do
    args <- getArgs
    case args of

        [ "-i", path ] -> do
            csv <- readFile path
            let parsed = parseCSV csv
            print parsed
            let fsm = initMachine parsed 
            print $ fsm 
            print $ eliminateUnreachableStates fsm
        
        [ "-i" ] -> do
            csv <- getContents
            print "meow"
            -- case (parseCSV csv) of
            --     Left err -> print err
            --     Right xs -> print $ initMachine xs

        _ -> print "Usage: dka-2-mka -i|-t [file_path]"
    
