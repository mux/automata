{-# LANGUAGE TypeFamilies #-}
module Data.Automaton.TiedDFA where

import Data.Automaton.Class
import Data.Map (Map)
import qualified Data.Map as M

-- A 'tied' DFA representation: transitions are expressed using a recursive
-- data type directly.  Such a representation is admittedly natural and very
-- elegant; however, since you cannot detect cycles in such a DFA (at least
-- not in plain Haskell), you cannot do anything with it except running it.
--

data TiedDFA a =
  TiedDFA (Map a (TiedDFA a))	-- Transitions to other states
          !Bool			-- Is this state final?

instance Ord a => AcceptFA (TiedDFA a) where
  type StateType (TiedDFA a) = TiedDFA a
  type InputType (TiedDFA a) = a

  initial                = id
  step  (TiedDFA ts _) x = const $ M.lookup x ts
  final (TiedDFA _ f)    = const f

-- DFA construction
unit :: Bool -> TiedDFA a
unit = TiedDFA M.empty

trans :: Ord a => a -> TiedDFA a -> TiedDFA a -> TiedDFA a
trans x d' (TiedDFA ts f) = TiedDFA (M.insert x d' ts) f
