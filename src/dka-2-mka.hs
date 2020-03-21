-- @project DKA-2-MKA
-- @author Bc. Matej Karas
-- @email xkaras34@stud.fit.vutbr.cz
-- @long xkaras34
-- @date 20.3.2020

import Text.ParserCombinators.Parsec hiding (State)
import System.Environment
import Data.List
import Data.Char

-- Syntactic sugar for type representing State
type State = String

-- Syntactic sugar for type representing Symbol
type Symbol = Char

-- Data type for Transition of FSM
data Transition = Transition {
  source :: State,
  symbol :: Symbol,
  dest :: State
} deriving (Eq, Show, Ord)

-- Data type for FSM
data FSM = FSM {
  states :: [State],
  alphabet :: [Symbol],
  initstate :: State,
  finalstates :: [State],
  transitions :: [Transition]
} deriving (Eq, Show)

---------------------------------------------- PARSER ----------------------------------------------

-- Defines parser file format
file :: GenParser Char st [[String]]
file = endBy line eol

-- Defines parser line format
line :: GenParser Char st [String]
line = sepBy cell (char ',')

-- Defines parser cell format
cell :: GenParser Char st String
cell = many (noneOf ",\n")

-- Defines parser end of line
eol :: GenParser Char st Char
eol = char '\n'

-- Parser for CSV input string file
parseCSV :: String -> [[String]]
parseCSV input = case parse file "(unknown)" input of
  Left err -> error $ show err
  Right xs -> return xs!!0

-------------------------------------------- FSM INIT ----------------------------------------------

-- Initialize Transition from Input string (parsed by CSV parser)
initTransitions :: [[String]] -> [Transition]
initTransitions [] = []
initTransitions (x:xs) 
  | length x == 3 = Transition {source = x!!0, dest = x!!2, symbol = head (x!!1)}:initTransitions xs
  | otherwise = error "Wrong format of transition!"

-- Initialize FSM from Input string (parsed by CSV parser)
initFSM :: [[String]] -> FSM
initFSM xs = checkFSM FSM {
  states = xs!!0,
  alphabet = if length (xs!!1) == 1 then head (xs!!1) else error "Wrong alphabet!",
  initstate = if length (xs!!2) == 1 then head (xs!!2) else error "Multiple init states!",
  finalstates = xs !! 3,
  transitions = initTransitions $ drop 4 xs
}

-- Checks validity of FSM 
checkFSM :: FSM -> FSM
checkFSM = stateDup . statesDigits . alphaChars . initCorrect . finalCorrect . transCorrect
  where 
    stateDup fsm = 
      if states fsm == nub (states fsm)
      then fsm else error "Input string contains duplicated states!"
    statesDigits fsm = 
      if foldr (\x xs -> not (null x) && all isDigit x && xs) True (states fsm) 
      then fsm else error "States contains non-digit characters!"
    alphaChars fsm = 
      if all (\x -> isAlpha x && isLower x) (alphabet fsm) 
      then fsm else error "Alphabet contains non-alpha symbols!"
    initCorrect fsm = 
      if initstate fsm `elem` states fsm 
      then fsm else error "Init state is not a valid state!"
    finalCorrect fsm = 
      if foldr (\x xs -> elem x (states fsm) && xs) True (finalstates fsm) 
      then fsm else error "Final state set is not valid!"
    transCorrect fsm = 
      if foldr (\x xs -> 
        elem (source x) (states fsm) && 
        elem (dest x) (states fsm) && 
        elem (symbol x) (alphabet fsm) ) True (transitions fsm) 
      then fsm else error "Transition set is invalid: invalid state/alphabet!"

-------------------------------------------- LOGIC -------------------------------------------------

-- Eliminates unreachable states of FSM 
eliminateUnreachableStates :: FSM -> FSM
eliminateUnreachableStates fsm = do
  let newStates = elim [initstate fsm] [] (transitions fsm)
  fsm { 
    states = newStates, 
    finalstates = finalstates fsm `intersect` newStates, 
    transitions = cleanTransitions newStates (transitions fsm) 
  }
  where 
    elim cur prev transition = let 
      new = nub $ foldr (\x xs -> transitionTo x transition ++ xs) cur cur
      in if new == prev then new else elim new cur transition
    transitionTo src = foldr (\x xs -> if source x == src then dest x:xs else xs) []
    cleanTransitions states = 
      foldr (\t ts -> if elem (source t) states && elem (dest t) states then t:ts else ts) []

-- makes totally defined FSM, by creating sink state, and fill all transitions, which are missing
makeTotallyDefined :: FSM -> FSM
makeTotallyDefined fsm 
  | null (undefPairs fsm) = fsm
  | otherwise = fsm { 
    states = "s":states fsm, 
    transitions = transitions fsm ++ createSinkTransitions fsm 
  }
  where
    genAllPairs fsm = 
      [(source, symbol) | source <- states fsm, symbol <- alphabet fsm]
    existingPairs fsm = map (\t -> (source t, symbol t)) (transitions fsm)
    undefPairs fsm = genAllPairs fsm \\ existingPairs fsm
    createSinkTransitions fsm = 
      map (\x -> uncurry Transition x "s") (undefPairs fsm ++ map (\x -> ("s", x)) (alphabet fsm))

-- Optimize FSM so that it will be minimal. First it sorts transitions, then it creates ekv classes
-- Afterwards, creates transitions for minimized FSM and sets the initstate to be 0
reduce :: FSM -> FSM
reduce fsm = do
  let sortedFsm = fsm { transitions = sort (transitions fsm) }
  let firstEkvClass = [finalstates fsm, states fsm \\ finalstates fsm]
  let finalClasses = initStateFirst $ makeClasses sortedFsm firstEkvClass
  fsm {
    states = map show [0 .. (length finalClasses - 1)],
    initstate = "0",
    finalstates = foldr (\x xs -> 
      if isFinal (finalstates fsm) x 
      then map show (elemIndices x finalClasses) ++ xs else xs
    ) [] finalClasses,
    transitions = foldr (\x xs -> 
      fixDestClasses finalClasses (makeTransitions x $ getClassIndexStr finalClasses $ head x) ++ xs
    ) [] finalClasses
  }
  where
    isFinal cls finalStates = foldr (\x xs -> elem x finalStates || xs) False cls
    fixDestClasses classes = map (\ x -> x{dest = getClassIndexStr classes $ dest x})
    makeTransitions cls src = 
      map (\ x -> Transition src x (getDest (head cls) x)) (alphabet fsm)
    getDest src sym = 
      dest $ (\(Just i) -> i) $ find (\x -> source x == src && symbol x == sym) (transitions fsm) 
    initStateFirst classes = x : pre ++ post
      where (pre, x:post) = splitAt (getClassIndex classes (initstate fsm)) classes

-------------------------------------------- HELPERS -----------------------------------------------

-- Creates List of ekv classes
makeClasses :: FSM -> [[State]] -> [[State]]
makeClasses fsm classes = do
  let new = foldr (\x xs -> split fsm x classes ++ xs) [] classes
  if classes == new 
  then classes else makeClasses fsm new
  where
    split fsm c classes = groupBy eq . sortBy cmp $ c
    getDestClasses fsm state = foldr (\x xs -> 
      if source x == state 
      then getClassIndex classes (dest x):xs else xs) [] (transitions fsm) 
    cmp x y = compare (getDestClasses fsm x) (getDestClasses fsm y)
    eq x y = getDestClasses fsm x == getDestClasses fsm y

-- this took longer than all the other stuffs in project -.-"
-- function fix transitions for automatic testing, so that destination states will be sorted
remapTransitions :: FSM -> FSM
remapTransitions fsm = let (cnd, a, b) = needSwap (take (length (states fsm)) (transitions fsm))
  in 
    if rint (dest $ head $ transitions fsm) > 1 
    then remapTransitions $ swapStates (dest $ head $ transitions fsm) "1" else 
      if cnd 
      then remapTransitions $ swapStates (show $ read a + 1) b else fsm
  where
    needSwap [t] = (False, "", "")
    needSwap (t1:t2:tx) | rint (dest t1) >= (rint (dest t2) - 1) = needSwap (t2:tx)
                        | otherwise = (True, dest t1, dest t2)
    swapStates a b = fsm {
      finalstates = sort $ foldr (\x xs -> 
        if x == a 
        then b:xs else 
          if x == b 
          then a:xs else x:xs) [] (finalstates fsm),
      transitions = sort $ map (\ x -> swapTrans x a b) (transitions fsm)
    }
    swapTrans tr a b = tr {
      source = if source tr == a then b else if source tr == b then a else source tr,
      dest = if dest tr == a then b else if dest tr == b then a else dest tr
    }

-- Returns class based on given State
getClass :: [[State]] -> State -> [State]
getClass classes state = classes !! getClassIndex classes state

-- Returns class based on given index of the State
getClassIndex :: [[State]] -> State -> Int
getClassIndex classes c = (\(Just i) -> i) $ findIndex (c `elem`) classes

-- Returns class baed on given index of the State as String
getClassIndexStr :: [[State]] -> State -> String
getClassIndexStr classes c = show $ (\(Just i) -> i) $ findIndex (c `elem`) classes

-- Return State as Integer value
rint :: State -> Int
rint = read

---------------------------------------- FORMATED PRINT --------------------------------------------
printFSM :: FSM -> IO()
printFSM fsm = sequence_ ([printStates, printAlphabet, printInit, printFinal] ++ printTransitions)
  where
    printStates = putStrLn $ intercalate "," (states fsm)
    printAlphabet = putStrLn $ alphabet fsm
    printInit = putStrLn $ initstate fsm
    printFinal = putStrLn $ intercalate "," (finalstates fsm)
    printTransitions = 
      map (\x -> putStrLn $ intercalate "," [source x, [symbol x], dest x]) (transitions fsm)

--------------------------------------------- MAIN -------------------------------------------------
main :: IO()
main = do
  args <- getArgs
  case args of

    [ "-i", path ] -> do
      csv <- readFile path
      printFSM $ initFSM $ parseCSV csv

    [ "-i" ] -> do
      csv <- getContents
      printFSM $ initFSM $ parseCSV csv

    [ "-t", path ] -> do
      csv <- readFile path
      let fsm = reduce $ makeTotallyDefined $ eliminateUnreachableStates $ initFSM $ parseCSV csv
      printFSM $ remapTransitions fsm

    [ "-t" ] -> do
      csv <- getContents
      let fsm = reduce $ makeTotallyDefined $ eliminateUnreachableStates $ initFSM $ parseCSV csv
      printFSM $ remapTransitions fsm
    
    _ -> print "Usage: dka-2-mka -i|-t [file_path]"
