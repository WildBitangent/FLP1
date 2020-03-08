import Text.ParserCombinators.Parsec hiding (State)
import System.Environment
import Data.List
import Data.Char

type State = String
type Symbol = Char

data Transition = Transition {
    source :: State,
    symbol :: Symbol,
    dest :: State
} deriving (Eq, Show, Ord)

data FSM = FSM {
        states :: [State],
        alphabet :: [Symbol],
        initstate :: State,
        finalstates :: [State],
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

--------------------------------------- FSM INIT ---------------------------------------
initTransitions :: [[String]] -> [Transition]
initTransitions [] = []
initTransitions (x:xs) 
    | length x == 3 = Transition { source = x!!0, dest = x!!2, symbol = head (x!!1) }:initTransitions xs
    | otherwise = error "Wrong format of transition!"

initFSM :: [[String]] -> FSM
initFSM xs = checkFSM FSM {
    states = xs!!0,
    alphabet = if length (xs!!1) == 1 then map (\x -> x) (head (xs!!1)) else error "Wrong alphabet!",
    initstate = if length (xs!!2) == 1 then head (xs!!2) else error "Multiple init states!",
    finalstates = xs !! 3,
    transitions = initTransitions $ drop 4 xs
}

checkFSM :: FSM -> FSM
checkFSM = stateDup . statesDigits . alphaChars . initCorrect . finalCorrect . transCorrect
    where 
        stateDup fsm = if states fsm == (nub $ states fsm) then fsm else error "Input string contains duplicated states!"
        statesDigits fsm = if foldr (\x xs -> length x > 0 && all isDigit x && xs) True (states fsm) 
            then fsm else error "States contains non-digit characters!"
        alphaChars fsm = if all (\x -> isAlpha x && isLower x) (alphabet fsm) then fsm else error "Alphabet contains non-alpha symbols!"
        initCorrect fsm = if elem (initstate fsm) (states fsm) then fsm else error "Init state is not a valid state!"
        finalCorrect fsm = if foldr (\x xs -> elem x (states fsm) && xs) True (finalstates fsm) then fsm else error "Final state set is not valid!"
        transCorrect fsm = if foldr (\x xs -> elem (source x) (states fsm) && elem (dest x) (states fsm) && elem (symbol x) (alphabet fsm) ) True (transitions fsm) 
            then fsm else error "Transition set is invalid: invalid state/alphabet!"

--------------------------------------- LOGIC -------------------------------------------
eliminateUnreachableStates :: FSM -> FSM
eliminateUnreachableStates fsm = do
    let newStates = elim [initstate fsm] [] (transitions fsm)
    fsm { states = newStates, finalstates = intersect (finalstates fsm) newStates, transitions = cleanTransitions newStates (transitions fsm) }
    where 
        elim cur prev transition = do
            let new = nub $ foldr (\x xs -> transitionTo x transition ++ xs) cur cur
            if new == prev then new else elim new cur transition
        transitionTo src transitions = foldr (\x xs -> if source x == src then dest x:xs else xs) [] transitions
        cleanTransitions states transitions = 
            foldr (\t ts -> if elem (source t) states && elem (dest t) states then t:ts else ts) [] transitions

makeTotallyDefined :: FSM -> FSM
makeTotallyDefined fsm 
    | undefPairs fsm == [] = fsm
    | otherwise = fsm { states = "s":states fsm, transitions = (transitions fsm) ++ (createSinkTransitions fsm) }
    where
        genAllPairs fsm = [(source, symbol) | source <- (states fsm), symbol <- foldr (\x xs -> [x] ++ xs) [] (alphabet fsm)]
        existingPairs fsm = map (\t -> (source t, symbol t)) (transitions fsm)
        undefPairs fsm = (genAllPairs fsm) \\ (existingPairs fsm)
        createSinkTransitions fsm = map (\x -> Transition (fst x) (snd x) "s") (undefPairs fsm ++ map (\x -> ("s", x)) (alphabet fsm))

reduce :: FSM -> FSM
reduce fsm = do
    let sortedFsm = fsm { transitions = sort (transitions fsm) }
    let finalClasses = initStateFirst $ makeClasses sortedFsm [finalstates fsm, states fsm \\ finalstates fsm]
    fsm {
        states = map show [0 .. (length finalClasses - 1)],
        initstate = show $ (\(Just i) -> i) $ findIndex (\x -> elem (initstate fsm) x) finalClasses,
        finalstates = foldr (\x xs -> if isFinal (finalstates fsm) x then (map show (elemIndices x finalClasses)) ++ xs else xs) [] finalClasses,
        transitions = foldr (\x xs -> (fixDestClasses finalClasses $ makeTransitions x $ getClassIndexStr finalClasses $ head x) ++ xs) [] finalClasses
    }
    where
        isFinal cls finalStates = foldr (\x xs -> elem x finalStates || xs) False cls
        makeTransitions cls src = foldr (\x xs -> (Transition src x (getDest (head cls) x)):xs) [] (alphabet fsm)
        fixDestClasses classes tr = foldr (\x xs -> x { dest = getClassIndexStr classes $ dest x }:xs) [] tr
        getDest src sym = dest $ (\(Just i) -> i) $ find (\x -> source x == src && symbol x == sym) (transitions fsm) 
        initStateFirst classes = x : pre ++ post
            where (pre, x:post) = splitAt (getClassIndex classes (initstate fsm)) classes

--------------------------------------- HELPERS -----------------------------------------
makeClasses :: FSM -> [[State]] -> [[State]]
makeClasses fsm classes = do
    let new = foldr (\x xs -> (split fsm x classes) ++ xs) [] classes
    if classes == new then classes
    else makeClasses fsm new
    where
        split fsm c classes = groupBy eq . sortBy cmp $ c
        getDestClasses fsm state = foldr (\x xs -> if source x == state then (getClassIndex classes $ dest x):xs else xs) [] (transitions fsm) -- returns list of dest states
        cmp x y = compare (getDestClasses fsm x) (getDestClasses fsm y)
        eq x y = (getDestClasses fsm x) == (getDestClasses fsm y)

getClassIndex :: [[State]] -> State -> Int
getClassIndex classes c = (\(Just i) -> i) $ findIndex (\x -> elem c x) classes

getClassIndexStr :: [[State]] -> State -> String
getClassIndexStr classes c = show $ (\(Just i) -> i) $ findIndex (\x -> elem c x) classes

--------------------------------------- MAIN -------------------------------------------
main :: IO()
main = do
    args <- getArgs
    case args of

        [ "-i", path ] -> do
            csv <- readFile path
            print $ initFSM $ parseCSV csv

        [ "-i" ] -> do
            csv <- getContents
            print $ initFSM $ parseCSV csv

        [ "-t", path ] -> do
            csv <- readFile path
            print $ reduce $ makeTotallyDefined $ eliminateUnreachableStates $ initFSM $ parseCSV csv

        [ "-t" ] -> do
            csv <- getContents
            print $ reduce $ makeTotallyDefined $ eliminateUnreachableStates $ initFSM $ parseCSV csv
        
        _ -> print "Usage: dka-2-mka -i|-t [file_path]"
