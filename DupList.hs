-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module DupList (
  DupList(..),
  dupListZipWith,
  dupListExpand, 
  dupListCreate
) where

--- A DupList is a run-length-encoded list.
--- Invariant: for any DupList run, the count (LHS)
--- of the run is always > 0.
newtype DupList a = DupList [ (Int, a) ]
    deriving (Show)

--- Alter the values in a DupList without changing the multiplicities.
--- The functor allows fmap to be defined.
instance Functor DupList where
    fmap f (DupList l) = DupList $ map (\(c, v)-> (c, f v)) l

--- Efficiently produce the DupList that would result from
--- expanding the given DupLists, applying zipWith, and
--- compressing the result back into a DupList.
dupListZipWith :: (Eq a, Eq b, Eq c) =>
                  (a -> b -> c) ->
                  DupList a ->
                  DupList b ->
                  DupList c
dupListZipWith _ (DupList []) _ = DupList []
dupListZipWith _ _ (DupList []) = DupList []
--- The trick here is to break up the longer run into pieces to fit
--- the shorter run.                   
dupListZipWith f (DupList ((c1, v1) : a)) (DupList ((c2, v2) : b)) =
    let c = min c1 c2
        a' = dupcons0 (c1 - c, v1) (DupList a)
        b' = dupcons0 (c2 - c, v2) (DupList b)
        rs = dupListZipWith f a' b' in
    dupconsv (c, f v1 v2) rs
    where
        --- cons on a new run, discarding 0-length runs
        dupcons0 (0, _) l = l
        dupcons0 v (DupList l) = DupList (v : l)
        --- cons on a new run, combining runs of equal value
        dupconsv (c1, v1) (DupList ((c2, v2) : rs)) | v1 == v2 =
            DupList ((c1 + c2, v1) : rs)
        dupconsv v (DupList l) = DupList (v : l)

dupListExpand :: DupList a -> [ a ]
dupListExpand (DupList l) =
    concatMap (uncurry replicate) l

dupListCreate :: [ (Int, a) ] -> DupList a
dupListCreate = DupList . filter ((/= 0) . fst)
