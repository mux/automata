module Data.Automaton.Levenshtein where

import Data.Automaton.NFA

-- Probably only useful for strings, but I kept a type
-- allowing arbitrary 'Ord a' inputs, just in case.
levenshtein :: Ord a => [a] -> Int -> NFA (Int,Int) a
levenshtein term k = finalize $ foldr trans (unit (0,0)) moves
  where len      = length term
        moves    = concatMap go (zip term [0..])
         where go (c,i) = concatMap go' [0..k]
                where go' j = ((i,j), Symbol c, (i+1,j)) :	-- Good input
                              if j >= k then [] else
                                [ ((i,j), Any, (i,j+1))		-- Deletion
                                , ((i,j), Any, (i+1,j+1))	-- Substitution
                                , ((i,j), Epsilon, (i+1,j+1))	-- Insertion
                                ]
        finalize nfa = foldr go nfa [0..k]
         where go i  = final (len, i) . if i >= k then id
                         else trans ((len,i), Any, (len,i+1))
