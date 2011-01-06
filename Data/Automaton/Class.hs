{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Data.Automaton.Class where

import Control.Applicative

class AcceptFA f where
  type StateType f :: *
  type InputType f :: *

  initial :: f -> StateType f
  step    :: f -> InputType f -> StateType f -> Maybe (StateType f)
  final   :: f -> StateType f -> Bool
  
data IntersectFA f g a = f :/\: g

instance (AcceptFA f, AcceptFA g, InputType f ~ InputType g) =>
         AcceptFA (IntersectFA f g a) where
  type StateType (IntersectFA f g a) = (StateType f, StateType g)
  type InputType (IntersectFA f g a) = InputType f

  initial (f :/\: g)        = (initial f, initial g)
  step (f :/\: g) x (q1,q2) = (,) <$> step f x q1 <*> step g x q2
  final (f :/\: g) (q1,q2)  = final f q1 && final g q2

data UnionFA f g a = f :\/: g

data OneOrBoth a b = First a
                   | Second b
                   | Both a b

instance (AcceptFA f, AcceptFA g, InputType f ~ InputType g) =>
         AcceptFA (UnionFA f g a) where
  type StateType (UnionFA f g a) = OneOrBoth (StateType f) (StateType g)
  type InputType (UnionFA f g a) = InputType f

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

run :: AcceptFA f => f -> [InputType f] -> (StateType f, [InputType f])
run f = go (initial f)
  where go q []         = (q, [])
        go q xxs@(x:xs) = maybe (q, xxs) (`go` xs) (step f x q)

accept :: AcceptFA f => f -> [InputType f] -> Bool
accept f = final f . fst . run f

intersect :: (AcceptFA f, AcceptFA g, InputType f ~ InputType g) =>
             f -> g -> IntersectFA f g (InputType f)
intersect = (:/\:)

union :: (AcceptFA f, AcceptFA g, InputType f ~ InputType g) =>
         f -> g -> UnionFA f g (InputType f)
union = (:\/:)
