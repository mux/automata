import NFA

-- Test DFAs that match 1*(0(1*)0(1*))*
dfa :: DFA Int Char
dfa = trans (2,'0',1) . trans (2,'1',2) .
      trans (1,'0',2) . trans (1,'1',1) .
      final 1 $ unit 1

dfa :: IntDFA Char
dfa = trans (2,'0',1) . trans (2,'1',2) .
      trans (1,'0',2) . trans (1,'1',1) .
      final 1 $ unit 1

dfa :: TiedDFA Char
dfa = s1
  where s1 = trans '0' s2 . trans '1' s1 $ unit True
        s2 = trans '0' s1 . trans '1' s2 $ unit False

-- Test NFA, matches (1*(01*01*)*) U (0*(10*10*)*)
-- Meaning, matches strings (containing only zeros and ones)
-- if the number of ones is even, or the number of zeros is.
nfa :: NFA Int Char
nfa = trans (1, Symbol '1', 1) . trans (1, Symbol '0', 2) .
      trans (2, Symbol '1', 2) . trans (2, Symbol '0', 1) .
      trans (3, Symbol '0', 3) . trans (3, Symbol '1', 4) .
      trans (4, Symbol '0', 4) . trans (4, Symbol '1', 3) .
      trans (0, Epsilon, 1) . trans (0, Epsilon, 3) .
      final 1 . final 3 $ unit 0

main :: IO ()
main = print $ determinize nfa
