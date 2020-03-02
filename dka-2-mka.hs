import Text.ParserCombinators.Parsec

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

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

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

main :: IO()
main = case (parseCSV "1,2,3\nabc\n1\n3\n1,a,3\n1,b,2\n2,a,2\n2,c,3\n") of
    Left err -> print err
    Right xs -> print $ initMachine xs
    
