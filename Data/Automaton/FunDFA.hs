{-# LANGUAGE TypeFamilies #-}
module Data.Automaton.FunDFA where

import Data.Automaton.Class

-- A DFA where we don't store states nor transitions but rely on
-- functions, just like in the mathematical definition of a DFA.

data FunDFA s a =
  FunDFA s			-- Initial state
         (a -> s -> Maybe s)	-- The transition function
         (s -> Bool)		-- Predicate for accepting states

-- The simplest embodiment of the AcceptFA typeclass.
instance AcceptFA (FunDFA s a) where
  type StateType (FunDFA s a) = s
  type InputType (FunDFA s a) = a

  initial (FunDFA q0 _ _) = q0
  step    (FunDFA _  s _) = s
  final   (FunDFA _  _ f) = f
