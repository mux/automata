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
class AcceptFA f a where
  -- The state type is parameterized over both 'fa' and 'a' to
  -- accomodate the recursive representation of tied DFAs.
  type StateType f a :: *

  initial :: f a -> StateType f a
  step    :: f a -> a -> StateType f a -> Maybe (StateType f a)
  final   :: f a -> StateType f a -> Bool

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

data IntersectFA f g a = f a :/\: g a

instance (AcceptFA f a, AcceptFA g a) => AcceptFA (IntersectFA f g) a where
  type StateType (IntersectFA f g) a = (StateType f a, StateType g a)

  initial (f :/\: g)        = (initial f, initial g)
  step (f :/\: g) x (q1,q2) = (,) <$> step f x q1 <*> step g x q2
  final (f :/\: g) (q1,q2)  = final f q1 && final g q2

data UnionFA f g a = f a :\/: g a

instance (AcceptFA f a, AcceptFA g a) => AcceptFA (UnionFA f g) a where
  type StateType (UnionFA f g) a =
    (Maybe (StateType f a), Maybe (StateType g a))

  initial (f :\/: g) = (Just (initial f), Just (initial g))
  step (f :\/: g) x (q1,q2)
    | isJust q1' || isJust q2' = Just (q1',q2')
    | otherwise                = Nothing
    where q1' = q1 >>= step f x
          q2' = q2 >>= step g x
  final (f :\/: g) (q1,q2) = maybe False (final f) q1 ||
                             maybe False (final g) q2

accept :: AcceptFA f a => f a -> [a] -> Bool
accept f = go (initial f)
  where go q []     = final f q
        go q (x:xs) = maybe False (`go` xs) (step f x q)

intersect :: (AcceptFA f a, AcceptFA g a) => f a -> g a -> IntersectFA f g a
intersect = (:/\:)

union :: (AcceptFA f a, AcceptFA g a) => f a -> g a -> UnionFA f g a
union = (:\/:)
