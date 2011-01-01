module Data.FiniteAutomata.TiedDFA where

import Data.Map (Map)
import qualified Data.Map as M

-- A 'tied' DFA representation: transitions are expressed using a recursive
-- data type directly.  Such a representation is admittedly natural and very
-- elegant; however, since you cannot detect cycles in such a DFA (at least
-- not in plain Haskell), you cannot do anything with it except running it.
--

data TiedDFA a =
  TiedDFA { transitions :: Map a (TiedDFA a)	-- Transitions to other states
          , final       :: !Bool		-- Is this state final?
          }

-- DFA construction
unit :: Bool -> TiedDFA a
unit = TiedDFA M.empty

trans :: Ord a => a -> TiedDFA a -> TiedDFA a -> TiedDFA a
trans x d' (TiedDFA ts f) = TiedDFA (M.insert x d' ts) f

-- Running a DFA
accept :: Ord a => TiedDFA a -> [a] -> Bool
accept dfa []     = final dfa
accept dfa (x:xs) = maybe False (`accept` xs) (step x dfa)

-- One step
step :: Ord a => a -> TiedDFA a -> Maybe (TiedDFA a)
step x = M.lookup x . transitions
