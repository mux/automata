{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Data.Automaton.Class where

import Control.Applicative

-- The typeclass takes two parameters so that I can constrain
-- the type 'a' to be an instance of 'Ord'.
class AcceptFA f a where
  -- The state type is parameterized over both 'fa' and 'a' to
  -- accomodate the recursive representation of tied DFAs.
  type StateType f a :: *

  initial :: f a -> StateType f a
  step    :: f a -> a -> StateType f a -> Maybe (StateType f a)
  final   :: f a -> StateType f a -> Bool

data IntersectFA f g a = f a :/\: g a

instance (AcceptFA f a, AcceptFA g a) => AcceptFA (IntersectFA f g) a where
  type StateType (IntersectFA f g) a = (StateType f a, StateType g a)

  initial (f :/\: g)        = (initial f, initial g)
  step (f :/\: g) x (q1,q2) = (,) <$> step f x q1 <*> step g x q2
  final (f :/\: g) (q1,q2)  = final f q1 && final g q2

data UnionFA f g a = f a :\/: g a

data OneOrBoth a b = First a
                   | Second b
                   | Both a b

instance (AcceptFA f a, AcceptFA g a) => AcceptFA (UnionFA f g) a where
  type StateType (UnionFA f g) a = OneOrBoth (StateType f a) (StateType g a)

  initial (f :\/: g) = Both (initial f) (initial g)

  step (f :\/: _) x (First q1)   = First <$> step f x q1
  step (_ :\/: g) x (Second q2)  = Second <$> step g x q2
  step (f :\/: g) x (Both q1 q2) = go (step f x q1) (step g x q2)
    where go Nothing   Nothing   = Nothing
          go (Just r1) Nothing   = Just (First r1)
          go Nothing   (Just r2) = Just (Second r2)
          go (Just r1) (Just r2) = Just (Both r1 r2)

  final (f :\/: _) (First q1)    = final f q1
  final (_ :\/: g) (Second q2)   = final g q2
  final (f :\/: g) (Both q1 q2)  = final f q1 || final g q2

run :: AcceptFA f a => f a -> [a] -> (StateType f a, [a])
run f = go (initial f)
  where go q []         = (q, [])
        go q xxs@(x:xs) = maybe (q, xxs) (`go` xs) (step f x q)

accept :: AcceptFA f a => f a -> [a] -> Bool
accept f = final f . fst . run f

intersect :: (AcceptFA f a, AcceptFA g a) => f a -> g a -> IntersectFA f g a
intersect = (:/\:)

union :: (AcceptFA f a, AcceptFA g a) => f a -> g a -> UnionFA f g a
union = (:\/:)
