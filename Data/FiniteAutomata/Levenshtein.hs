module Data.FiniteAutomata.Levenshtein where

import Data.FiniteAutomata.NFA

-- Probably only useful for strings, but I kept a type
-- allowing arbitrary 'Ord a' inputs anyway.
levenshtein :: Ord a => [a] -> Int -> NFA (Int,Int) a
levenshtein term k = finalize . build $ unit (0,0)
  where len          = length term
        finalize nfa = foldr go nfa [0..k]
          where go i = final (len, i) . if i >= k then id
                         else trans ((len,i), Any, (len,i+1))
        build nfa    = foldr go nfa (zip term [0..])
        go (c,i) nfa = foldr go' nfa [0..k]
          where go' j = trans ((i,j), Symbol c, (i+1,j)) .	-- Good input
                        if j >= k then id else
                          trans ((i,j), Any, (i,j+1)) .		-- Deletion
                          trans ((i,j), Any, (i+1,j+1)) .	-- Substitution
                          trans ((i,j), Epsilon, (i+1,j+1))	-- Insertion
