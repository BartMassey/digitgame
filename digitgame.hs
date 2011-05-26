-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-
#
# digitgame
# Copyright 2005-2007 Bart Massey <bart@cs.pdx.edu> and
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
import Monad
import Char
import Data.Maybe
import Random
import Array
import IO
import Numeric
import Ratio
import System.Console.ParseArgs

import DupList
import Subseqs

data Options =
    OptionASCII |
    OptionManualRoll |
    OptionThreshold
    deriving (Ord, Eq, Show)

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
         ]

printRules :: IO ()
printRules = mapM_ putStrLn rules

digits = ['1'..'9']

scoreList :: [ String ]
scoreList = subseqs digits

rollSize :: Int
rollSize = length scoreList

allScores :: Array Int String
allScores = array (1, rollSize) (zip [1 .. rollSize] scoreList)

type State = String
type Vtype = Rational
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
dprob =
    let counts = [(a + b, 1) | a <- [1 .. 6], b <- [1 .. 6]]
        mults = accumArray (+) 0 (2, 12) counts in
    map (% 36) (elems mults)

--- Return a list of all possible die rolls that achieve
--- totals of 2,3..,12.
--- Strategy: Given a state, first build all subsets of that
--- state.  For each such subset, replace it with a pair
--- consisting of the sum of its digits (the bin index) and
--- the subset itself.  Filter out those pairs whose index
--- is not in the range 2..12 since these are unrealizable
--- by die rolls.  Finally, build a histogram of lists of
--- states indexed by die roll value.  Each bin in the
--- histogram is thus the set of rolls that achieve the
--- given total.
rolls :: State -> Array Int [ State ]
rolls =
    accumArray (flip (:)) [] (2, 12) .
    filter (inRange (2, 12) . fst) .
    map (\set -> (sum (map digitToInt set), set)) .
    subseqs

--- Compute the value of a state.  This is the heart of the
--- calculation.
--- Strategy: The value of a state at a given scoring
--- threshold on a given roll is the maximum value of its
--- child states at that threshold and roll, weighted
--- appropriately.  For rolls for which there is no child,
--- the value of the state at that roll is just its score.
value :: State -> Ptable
value target_state =
    let sum_ptable index state =
            let score = dupListCreate [(511 - index, 1), (1, 1%2), (index, 0)]
                values rs = score : [ value (state \\ r) | r <- rs ]
                ptable_maximize rs = ptableFold max (values rs)
                max_ptable = map ptable_maximize (elems (rolls state)) in
            ptableFold (+) (zipWith (fmap . (*)) dprob max_ptable)
        score_state (score, state) = (state, sum_ptable score state)
        final_values = map score_state (zip [0 ..] scoreList) in
    fromJust (lookup target_state final_values)

--- remove whitespace from beginning and end of string
trimWhite :: String -> String
trimWhite =
    reverse . trim_head . reverse . trim_head
    where
      trim_head = dropWhile (\c -> c == ' ' || c == '\t')

--- Prompt and return trimmed string
getInput :: String -> IO String
getInput prompt = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  putStr prompt
  putStr " "
  r <- getLine
  return (trimWhite r)

--- Prompt and return Int in range
getIntInput :: String -> (Int, Int) -> IO Int
getIntInput prompt range@(low, high) = do
  s <- getInput prompt
  when (s == "q") (error "user quit")
  let tryAgain = putStrLn "?" >> getIntInput prompt range
  let ue e = if isUserError e then tryAgain else ioError e
  t <- ((readIO s) :: IO Int) `catch` ue
  if (t >= low) && (t <= high)
     then return t
     else tryAgain

--- Prompt and return Letter in range
getLetterInput :: String -> Int -> IO Int
getLetterInput prompt high = do
  s <- getInput prompt
  when (s == "q") (error "user quit")
  let v = ord (head s) - ord 'a'
  let tryAgain = putStrLn "?" >> getLetterInput prompt high
  if length s == 1 && v >= 0 && v < high
     then return v
     else tryAgain

--- iterate f on states to termination
while :: Monad m => (a -> m a) -> a -> m ()
while f x = f x >>= while f

--- from the Haskell 98 report
rollDie :: IO Int
rollDie = getStdRandom (randomR (1,6))

rollDice :: IO Int
rollDice =
    do  r1 <- rollDie
        r2 <- rollDie
        return (r1 + r2)

atoi :: String -> Int
atoi "" = 0
atoi s  = read s

--- Shuffle a list
--- Knuth et al algorithm
shuffle :: [ b ] -> IO [ b ]
shuffle [] = return []
shuffle [x] = return [x]
shuffle s = do
  r <- getStdRandom (randomR (0, length s - 1))
  let (lt, rt) = splitAt r s
  more <- shuffle (lt ++ tail rt)
  return (head rt : more)

--- Play the game
--- Supplied rollers let user enter die rolls manually or
--- roll dice randomly
play :: IO Int -> Int -> IO ()
play roller threshold =
    while playRound digits
    where
        --- Handle continuing game.
        playRound :: State -> IO State
        playRound cur = do
          roll <- roller
          putStrLn (cur ++ " ... " ++ (show roll))
          let moves = reverse ((rolls cur) ! roll)
          if (length moves) == 0
             then endGame
             else nextMove moves
          where
            --- Handle end of game.
            endGame :: IO State
            endGame = do
              when (cur == "") (putStrLn "Perfect game!")
              let threshold_score = allScores ! threshold
              case compare (atoi cur) (atoi threshold_score) of
                LT -> fail "You win!"
                GT -> fail "You lose."
                EQ -> fail "A tie."
            --- Actually step through a move
            nextMove :: [ State ] -> IO State
            nextMove moves = do
              let l = length moves
              smoves <- shuffle moves
              showMoves smoves
              m <- case l of
                     1 -> do
                        putStrLn "c> a"
                        return (head smoves)
                     _ -> do
                        r <- getLetterInput "c>" l
                        let m = smoves !! r
                        valueMoves moves m
                        return m
              return (cur \\ m)
              where
                --- Print the move list
                showMoves :: [ State ] -> IO ()
                showMoves moves =
                    zipWithM_ showMove ['a' ..] moves
                    where
                      showMove letter take = 
                          putStrLn ((letter : ") ") ++
                                    take ++ " -> " ++ (cur \\ take))
                --- Print the move values
                valueMoves :: [ State ] -> State -> IO ()
                valueMoves moves choice =
                    mapM_ valueMove moves
                    where
                      valueMove take = 
                          putStrLn ((choiceChar take) ++
                                    take ++ " -> " ++
                                    (cur \\ take) ++ " = " ++
                                    (choiceVal take))
                      choiceChar take =
                          if take == choice
                          then "*"
                          else " "
                      choiceVal take =
                          let vlist = value (cur \\ take)
                              vals = dupListExpand vlist
                              elem = vals !! (512 - threshold) in
                          showDecimalRat elem
                      showDecimalRat r =
                          showFFloat (Just 4) (fromRat r) ""

argd :: [ Arg Options ]
argd = [ Arg { argIndex = OptionASCII,
               argName = Just "ascii",
               argAbbr = Just 'a',
               argData = Nothing,
               argDesc = "Use ASCII UI" },
         Arg { argIndex = OptionManualRoll,
               argName = Just "manual-roll",
               argAbbr = Just 'm',
               argData = Nothing,
               argDesc = "Dice will be rolled manually and input" },
         Arg { argIndex = OptionThreshold,
               argName = Just "threshold",
               argAbbr = Just 't',
               argData = argDataRequired "target-value" ArgtypeInt,
               argDesc = "Compute probability of reaching target-value" }]

main =
    do  args <- parseArgsIO ArgsComplete argd
        let threshold = fromJust (getArgInt args OptionThreshold)
        unless (threshold >= 1 && threshold <= rollSize)
               (usageError args ("threshold out of range 1.." ++
                                 show rollSize))
        putStrLn ("threshold = " ++ (allScores ! threshold))
        let roller = if gotArg args OptionManualRoll
                     then getIntInput "r>" (2,12)
                     else rollDice
        play roller threshold `catch` (putStrLn . ioeGetErrorString)
        putStrLn "Thanks for playing!"

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
