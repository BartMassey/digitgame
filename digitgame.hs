{-
#
# digitgame
# Copyright 2005 Bart Massey <bart@cs.pdx.edu> and
# Jamey Sharp <jamey@cs.pdx.edu>
# ALL RIGHTS RESERVED
# See the end of this file for licensing information
#
# Game with dice and digits (a wooden game owned
# by the Andreas Junghanns family)
#
-}

module Main where

import List
import Char
import qualified Data.Map as Map
import Random
import Array
import IO

digits = ['1'..'9']

rules :: [ String ]
rules = ["Rules:",
	 "  A round is played solo, as follows",
         "    1. start with the sequence of digits 1..9",
         "    2. roll two 6-sided dice to obtain a sum 2..12",
         "    3. if there exists a set of digits in the sequence that add",
         "       up to that sum",
         "       3.1 delete some such set of digits",
         "       3.2 go to 2.",
         "    4. score is the remaining sequence, treated as a place value",
         "       number",
         "  Each player plays a round in turn.  If any player scores",
         "  0, they win immediately.  Otherwise, low scorer wins.",
         "",
         "This code gives equities for optimal solo play."
---	 "This code gives equities (in terms of fraction of loss=-1.0",
---	 ".. win=1.0) for optimal two-player play."
         ]

printRules :: IO ()
--- mapM_ is the monadic map that discards the resulting list.
printRules = mapM_ putStrLn rules

--- A DupList is a run-length-encoded list.
--- Invariant: for any DupList, the count (LHS)
--- of the run is always > 0.
newtype DupList a = DupList [ (Int, a) ]
    deriving (Show)

--- Alter the values in a DupList without changing the multiplicities.
--- The functor allows fmap to be defined.
instance Functor DupList where
    fmap f (DupList l) = DupList $ map (\(c, v)-> (c, f v)) l


dupListZipWith :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> DupList a -> DupList b -> DupList c
dupListZipWith _ (DupList []) _ = DupList []
dupListZipWith _ _ (DupList []) = DupList []
--- The trick here is to break up the longer run into pieces to fit
--- the shorter run.                   
dupListZipWith f (DupList ((c1, v1):a)) (DupList ((c2, v2):b)) =
        dupconsv (c, v1 `f` v2) $ dupListZipWith f
            (dupcons0 (c1 - c, v1) (DupList a))
            (dupcons0 (c2 - c, v2) (DupList b))
    where
        c = min c1 c2
        --- Add an element onto the front of a DupList,
        --- combining multiplicities appropriately.
        dupcons0 :: Eq a => (Int, a) -> DupList a -> DupList a
        dupcons0 (0,_) l = l
        dupcons0 v (DupList l) = DupList $ v:l
        dupconsv v (DupList l@[]) = DupList $ v:l
        dupconsv v@(c1,v1) (DupList l@((c2, v2):rem))
            | v1 == v2 = DupList $ (c1 + c2, v2):rem
            | otherwise = DupList $ v:l

dupListExpand :: DupList a -> [ a ]
dupListExpand (DupList []) = []
dupListExpand (DupList ((count, elem) : dls)) =
    (replicate count elem) ++ (dupListExpand (DupList dls))


--- return all subsequences of
--- a given length, in lex order.
subseqsLen :: [a] -> Int -> [[a]]
subseqsLen _ 0 = [[]]
subseqsLen es l | length es < l = []
subseqsLen (e:es) l =
    (map (e:) (subseqsLen es (l - 1))) ++
    (subseqsLen es l)
        
--- return all subsequences, in
--- length-lex order.
subseqs :: [a] -> [[a]]
subseqs l =
    concatMap (subseqsLen l) [0..(length l)]

type State = String

--- type Vtype = Rational
type Vtype = Double

type Ptable = DupList Vtype

--- Fold up a list of ptables into a single ptable
--- by using the given combining operator entry-by-entry.
--- Give an error if there are no tables to combine;
--- the way this is used that should never happen.
ptableFold :: (Vtype -> Vtype -> Vtype) -> [ Ptable ] -> Ptable
ptableFold = foldl1 . dupListZipWith

--- Returns a histogram of the probabilities for each
--- of the 11 possible unique die roll values.
dprob :: [ Vtype ]
dprob = fmap (/ toEnum (length twodie)) $
        (elems . (accumArray (flip (+)) 0 (minimum twodie, maximum twodie))) $
        map (\v->(v,1)) twodie
    where twodie = [a + b | a<-[1..6], b<-[1..6]]

--- Return a list of all possible die rolls that
--- achieve totals of 2,3..,12.
rolls :: State -> Array Int [ State ]
--- Strategy:  Given a state, first build all subsets
--- of that state.  For each such subset, replace it with
--- a pair consisting of the sum of its digits (the bin index)
--- and the subset itself.  Filter out those pairs whose index
--- is not in the range 2..12 since these are unrealizable by
--- die rolls.  Finally, build a histogram of lists of states indexed
--- by die roll value.  Each bin in the histogram is thus the
--- set of rolls that achieve the given total.  
rolls = accumArray (flip (:)) [] range .
        filter (inRange range . fst) .
        map (\set-> (sum $ map digitToInt set, set)) .
        subseqs
    where range = (2, 12)

--- The value of a state at a given scoring threshold on a
--- given roll is the maximum value of its child states at
--- that threshold and roll, weighted appropriately.  For
--- rolls for which there is no child, the value of the
--- state at that roll is just its score.
value :: State -> Ptable
value state = (\(Just p)-> p) $ Map.lookup state values
   where
        values :: Map.Map State Ptable
        values = Map.fromList $
            map (\(score, state)-> (state, value' score state)) $
            zip [0..] $ subseqs digits

        value' :: Int -> State -> Ptable
        value' index state = ptableFold (+) $
            zipWith (fmap . (*)) dprob $
            map valuehelper $
            elems (rolls state)
            where
                sc = DupList $ filter ((/= 0) . fst)
                    [(511 - index, 1), (1, 1/2), (index, 0)]

                valuehelper :: [ State ] -> Ptable
                valuehelper rs = ptableFold max $
                    sc : [ value $ state \\ r | r <- rs ]

--- for testing
--- main = print $ value digits

--- remove whitespace from beginning and end of string
trimWhite :: String -> String
trimWhite =
    reverse . trimWhiteHead . reverse . trimWhiteHead
    where
        --- remove whitespace from beginning of string
        trimWhiteHead :: String -> String
        trimWhiteHead (' ':ss) = trimWhite ss
        trimWhiteHead ('\t':ss) = trimWhite ss
        trimWhiteHead ss = ss

--- Prompt and return trimmed string
getInput :: String -> IO String
getInput prompt =
    do  hSetBuffering stdin LineBuffering
        putStr prompt
        putStr " "
        r <- getLine
        return (trimWhite r)

--- Prompt and return Int in range
getIntInput :: String -> (Int, Int) -> IO Int
getIntInput prompt range@(low, high) =
    do  s <- getInput prompt
        if s == "q"
            then error "user quit"
            else
                do
                    t <- ((readIO s) :: IO Int) `catch`
                         (\e ->
                              if isUserError e
                                  then tryAgain
                                  else ioError e)
                    if (t >= low) && (t <= high)
                        then return t
                        else tryAgain
    where
        tryAgain = putStrLn "?" >> getIntInput prompt range

--- Prompt and return Letter in range
getLetterInput :: String -> Int -> IO Char
getLetterInput prompt high =
    do  s <- getInput prompt
        if s == "q"
            then error "user quit"
            else
                if (length s == 1) && ((head s) >= 'a') &&
                   ((head s) <= chr((ord 'a') + (high - 1)))
                    then return (head s)
                    else tryAgain
    where
        tryAgain = putStrLn "?" >> getLetterInput prompt high

scoreList :: [ String ]
scoreList = subseqs digits

rollSize :: Int
rollSize = length scoreList

allScores :: Array Int String
allScores =
    array (1, rollSize) (zip [1..rollSize] scoreList)

while :: (a -> IO (Maybe a)) -> a -> IO ()
while f x =
    do  y <- f x
        case y of
            Just z -> while f z
            Nothing -> return ()

--- from the Haskell 98 report
rollDie :: IO Int
rollDie = getStdRandom (randomR (1,6))

rollDice :: IO Int
rollDice =
    do  r1 <- rollDie
        r2 <- rollDie
        return (r1 + r2)

atoi :: String -> Int
atoi = read

--- (current position, score threshold)
type GState = (String, Int)

--- Shuffle a list
--- Knuth et al algorithm
shuffle :: [ b ] -> IO [ b ]
shuffle [] = return []
shuffle [x] = return [x]
shuffle s =
    do  r <- getStdRandom (randomR (0, (length s) - 1))
        let (lt, rt) = splitAt r s in
            do  more <- shuffle (lt ++ (tail rt))
                return ((head rt) : more)

--- Play the game
--- Autoroll argument false indicates
--- user will enter die rolls manually.
play :: Bool -> IO ()
play autoroll =
    do  threshold <- getIntInput "t>" (1, rollSize)
        putStrLn ("threshold = " ++ (allScores ! threshold))
        while playRound (digits, threshold)
        where
            --- Handle end of game.
            endGame :: Int -> IO (Maybe GState)
            endGame sign =
                do  if sign < 0
                        then putStrLn "You win!"
                        else if sign > 0
                        then putStrLn "You lose."
                        else putStrLn "A tie."
                    putStrLn "Thanks for playing!"
                    return Nothing
            --- Handle continuing game.
            playRound :: GState -> IO (Maybe GState)
            playRound (cur, threshold) =
                do  roll <- if autoroll
                                then rollDice
                                else (getIntInput "r>" (2,12))
                    putStrLn (cur ++ " ... " ++ (show roll))
                    let  moves = ((rolls cur) ! roll) in
                         if (length moves) == 0
                             then endGame ((atoi cur) -
                                           (atoi (allScores ! threshold)))
                             else nextMove moves
                where
                    -- Actually step through a move
                    nextMove :: [ State ] -> IO (Maybe GState)
                    nextMove moves =
                        let l = length moves in
                            do  smoves <- shuffle moves
                                showMoves smoves
                                if l == 1
                                    then
                                        do  putStrLn "c> a"
                                            return (Just
                                                    ((cur \\ (head smoves)),
                                                     threshold))
                                    else
                                        do  r <- getLetterInput "c>" l
                                            let m = smoves !!
                                                    ((ord r) - (ord 'a')) in
                                                do
                                                    valueMoves moves m
                                                    return (Just (cur \\ m,
                                                            threshold))
                        where
                            --- Print the move list
                            showMoves :: [ State ] -> IO ()
                            showMoves moves =
                                mapM_ showMove (zip ['a'..] moves)
                                where
                                    showMove (letter, take) = 
                                        putStrLn ((letter : ") ") ++
                                                  take ++
                                                  " -> " ++
                                                  (cur \\ take))
                            --- Print the move values
                            valueMoves :: [ State ] -> State -> IO ()
                            valueMoves moves choice =
                                mapM_ valueMove moves
                                where
                                    valueMove take = 
                                        putStrLn ((choiceChar take) ++
                                                  take ++
                                                  " -> " ++
                                                  (cur \\ take) ++
                                                  " = " ++
                                                  (choiceVal take))
                                    choiceChar take =
                                        if take == choice
                                            then "*"
                                            else " "
                                    choiceVal take =
                                        show ((dupListExpand
                                            (value (cur \\ take))) !!
                                            threshold)

main = play True

{-
	if (cur == "") {
	    printf("Perfect game!\n");
	    printf("thanks for playing!\n");
	    exit(0);
	}
-}

{-
  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated
  documentation files (the "Software"), to deal in the
  Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute,
  sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so,
  subject to the following conditions:
  
  The above copyright notice and this permission notice shall
  be included in all copies or substantial portions of the
  Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
  PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.
  
  Except as contained in this notice, the names of the authors
  or their institutions shall not be used in advertising or
  otherwise to promote the sale, use or other dealings in this
  Software without prior written authorization from the
  authors.
-}
