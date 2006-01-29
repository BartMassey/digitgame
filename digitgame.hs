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

import qualified Array
import qualified List
import qualified Char
import qualified Data.Map as Map

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

--- Generalized histogram.  Given an "increment" operator that
--- takes the old value of the bin and a new thing to be placed
--- in the bin and returns new contents for that bin;
--- an identity to initialize each bin; an interval denoting the
--- range of bin indices; and a list of things to put in designated
--- bins. Produces a list of final bin contents in index order.
accum :: Array.Ix i => (a -> e -> e) -> e -> (i, i) -> [(i, a)] -> [e]
--- XXX accumArray takes its function argument's arguments
--- "backwards" from all sensibility.
accum f z r = Array.elems . Array.accumArray (flip f) z r

--- Returns a histogram of the probabilities for each
--- of the 11 possible unique die roll values.
dprob :: [ Vtype ]
dprob = fmap (/ toEnum (length twodie)) $
        accum (+) 0 (minimum twodie, maximum twodie) $
        map (\v->(v,1)) twodie
    where twodie = [a + b | a<-[1..6], b<-[1..6]]

--- Return a list of all possible die rolls that
--- achieve totals of 2,3..,12.
rolls :: State -> [ [ State ] ]
--- Strategy:  Given a state, first build all subsets
--- of that state.  For each such subset, replace it with
--- a pair consisting of the sum of its digits (the bin index)
--- and the subset itself.  Filter out those pairs whose index
--- is not in the range 2..12 since these are unrealizable by
--- die rolls.  Finally, build a histogram of lists of states indexed
--- by die roll value.  Each bin in the histogram is thus the
--- set of rolls that achieve the given total.  
rolls = accum (:) [] range .
        filter (Array.inRange range . fst) .
        map (\set-> (sum $ map Char.digitToInt set, set)) .
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
            zipWith (fmap . (*)) dprob $ map valuehelper $ rolls state
            where
                sc = DupList $ filter ((/= 0) . fst)
                    [(511 - index, 1), (1, 1/2), (index, 0)]

                valuehelper :: [ State ] -> Ptable
                valuehelper rs = ptableFold max $
                    sc : [ value $ state List.\\ r | r <- rs ]

--- main = print $ value digits

--- Prompt and return string
getInput :: String -> IO String
getInput prompt =
    do putStr prompt;
       putStr " ";
       r <- getLine;
       return r
