-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Subseqs (subseqs)
where

--- return all subsequences of
--- a given length, in lex order.
subseqsLen :: [a] -> Int -> [[a]]
subseqsLen _ 0 = [[]]
subseqsLen es l | length es < l = []
subseqsLen (e : es) l =
    let eseqs = map (e :) (subseqsLen es (l - 1)) in
    eseqs ++ subseqsLen es l
        
--- return all subsequences, in
--- length-lex order.
subseqs :: [a] -> [[a]]
subseqs l = concatMap (subseqsLen l) [0 .. length l]
