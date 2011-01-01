import Data.FiniteAutomata.DFA (DFA)
import qualified Data.FiniteAutomata.DFA as DFA
import Data.FiniteAutomata.IntDFA (IntDFA)
import qualified Data.FiniteAutomata.IntDFA as IDFA
import Data.FiniteAutomata.TiedDFA (TiedDFA)
import qualified Data.FiniteAutomata.TiedDFA as TDFA
import Data.FiniteAutomata.NFA (NFA, Input(..))
import qualified Data.FiniteAutomata.NFA as NFA

-- Test DFAs that match 1*(0(1*)0(1*))*
dfa :: DFA Int Char
dfa = DFA.trans (2,DFA.Symbol '0',1) . DFA.trans (2,DFA.Symbol '1',2) .
      DFA.trans (1,DFA.Symbol '0',2) . DFA.trans (1,DFA.Symbol '1',1) .
      DFA.final 1 $ DFA.unit 1

dfa2 :: IntDFA Char
dfa2 = IDFA.trans (2,'0',1) . IDFA.trans (2,'1',2) .
       IDFA.trans (1,'0',2) . IDFA.trans (1,'1',1) .
       IDFA.final 1 $ IDFA.unit 1

dfa3 :: TiedDFA Char
dfa3 = s1
  where s1 = TDFA.trans '0' s2 . TDFA.trans '1' s1 $ TDFA.unit True
        s2 = TDFA.trans '0' s1 . TDFA.trans '1' s2 $ TDFA.unit False

-- Test NFA, matches (1*(01*01*)*) U (0*(10*10*)*)
-- Meaning, matches strings (containing only zeros and ones)
-- if the number of ones is even, or the number of zeros is.
nfa :: NFA Int Char
nfa = NFA.trans (1, NFA.Symbol '1', 1) . NFA.trans (1, NFA.Symbol '0', 2) .
      NFA.trans (2, NFA.Symbol '1', 2) . NFA.trans (2, NFA.Symbol '0', 1) .
      NFA.trans (3, NFA.Symbol '0', 3) . NFA.trans (3, NFA.Symbol '1', 4) .
      NFA.trans (4, NFA.Symbol '0', 4) . NFA.trans (4, NFA.Symbol '1', 3) .
      NFA.trans (0, NFA.Epsilon, 1) . NFA.trans (0, NFA.Epsilon, 3) .
      NFA.final 1 . NFA.final 3 $ NFA.unit 0

main :: IO ()
main = print $ NFA.determinize nfa
