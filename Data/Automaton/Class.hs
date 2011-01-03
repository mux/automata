{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Data.Automaton.Class where

import Control.Applicative
import Data.Maybe
import Data.Automaton.DFA (DFA)
import qualified Data.Automaton.DFA as DFA
import Data.Automaton.IntDFA (IntDFA)
import qualified Data.Automaton.IntDFA as IDFA
import Data.Automaton.TiedDFA (TiedDFA)
import qualified Data.Automaton.TiedDFA as TDFA
import Data.Automaton.NFA (NFA)
import qualified Data.Automaton.NFA as NFA
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

instance (Ord a, Ord s) => AcceptFA (DFA s) a where
  type StateType (DFA s) a = s

  initial     = DFA.start
  step        = DFA.step
  final fa q  = q `S.member` DFA.finals fa

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
    -- This probably adds some overhead, but NFAs are not intended to
    -- be run directly anyways, and should be converted to DFAs first.
    where wrap qs = if S.null qs then Nothing else Just qs
  final fa qs = not . S.null $ qs `S.union` NFA.finals fa

data IntersectFA x y a = x a :/\: y a

instance (AcceptFA x a, AcceptFA y a) => AcceptFA (IntersectFA x y) a where
  type StateType (IntersectFA x y) a = (StateType x a, StateType y a)

  initial (x :/\: y)        = (initial x, initial y)
  step (x :/\: y) i (q1,q2) = (,) <$> step x i q1 <*> step y i q2
  final (x :/\: y) (q1,q2)  = final x q1 && final y q2

data UnionFA x y a = x a :\/: y a

instance (AcceptFA x a, AcceptFA y a) => AcceptFA (UnionFA x y) a where
  type StateType (UnionFA x y) a =
    (Maybe (StateType x a), Maybe (StateType y a))

  initial (x :\/: y) = (Just (initial x), Just (initial y))
  step (x :\/: y) i (q1,q2)
    | isJust q1' || isJust q2' = Just (q1',q2')
    | otherwise                = Nothing
    where q1' = q1 >>= step x i
          q2' = q2 >>= step y i
  final (x :\/: y) (q1,q2) = maybe False (final x) q1 ||
                             maybe False (final y) q2

accept :: AcceptFA fa a => fa a -> [a] -> Bool
accept fa = go (initial fa)
  where go q []     = final fa q
        go q (x:xs) = maybe False (`go` xs) (step fa x q)

intersect :: (AcceptFA x a, AcceptFA y a) => x a -> y a -> IntersectFA x y a
intersect = (:/\:)

union :: (AcceptFA x a, AcceptFA y a) => x a -> y a -> UnionFA x y a
union = (:\/:)
