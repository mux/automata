{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Data.FiniteAutomata.Class where

import Data.FiniteAutomata.DFA (DFA)
import qualified Data.FiniteAutomata.DFA as DFA
import Data.FiniteAutomata.IntDFA (IntDFA)
import qualified Data.FiniteAutomata.IntDFA as IDFA
import Data.FiniteAutomata.TiedDFA (TiedDFA)
import qualified Data.FiniteAutomata.TiedDFA as TDFA
import Data.FiniteAutomata.NFA (NFA)
import qualified Data.FiniteAutomata.NFA as NFA
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IntSet as IS

-- The typeclass takes two parameters so that I can constrain
-- the type 'a' to be an instance of 'Ord'.
class AcceptFA fa a where
  -- The state type is parameterized over both 'fa' and 'a' to
  -- accomodate the recursive representation of tied DFAs.
  type StateType fa a :: *

  initial :: fa a -> StateType fa a
  step    :: fa a -> a -> StateType fa a -> Maybe (StateType fa a)
  final   :: fa a -> StateType fa a -> Bool

  --with :: fa a -> a -> StateType fa a -> b -> (StateType fa a -> b) -> b

instance (Ord a, Ord s) => AcceptFA (DFA s) a where
  type StateType (DFA s) a = s

  initial     = DFA.start
  step        = DFA.step
  final fa q  = q `S.member` DFA.finals fa

  --with fa x q y f = maybe y f (DFA.step fa x q)

instance Ord a => AcceptFA IntDFA a where
  type StateType IntDFA a = IDFA.State	-- Int

  initial    = IDFA.start
  step       = IDFA.step
  final fa q = q `IS.member` IDFA.finals fa

instance Ord a => AcceptFA TiedDFA a where
  type StateType TiedDFA a = TiedDFA a

  initial     = id
  step _      = TDFA.step
  final _ q   = TDFA.final q

instance (Ord a, Ord s) => AcceptFA (NFA s) a where
  type StateType (NFA s) a = Set s

  initial     = S.singleton . NFA.start
  step fa x   = wrap . NFA.step fa x
    where wrap qs = if S.null qs then Nothing else Just qs
  final fa qs = not . S.null $ qs `S.union` NFA.finals fa

accept :: AcceptFA fa a => fa a -> [a] -> Bool
accept fa = go (initial fa)
  where go q []     = final fa q
        go q (x:xs) = maybe False (`go` xs) (step fa x q)
