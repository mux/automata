{-# LANGUAGE TypeFamilies #-}
module Data.Automaton.DFA where

import Control.Applicative
import Data.Automaton.Class
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- Another representation of a DFA convenient for constructing one
-- from scratch, that also allows to navigate within the DFA without
-- entering cycles.
--
-- The DFA is entirely defined by the transitions within the states,
-- along with a set a final states.

-- 'Default' moves are not part of the theory, but they arise naturally
-- when converting from NFAs with 'Any' moves.
data Input a = Symbol a
             | Default
  deriving (Eq,Ord,Show)

type Transition s a  = (s, Input a, s)	-- One single transition
type Transitions s a = Map (Input a) s	-- Outgoing transitions of a state

data DFA s a =
  DFA { start       :: s			-- Initial state
      , transitions :: Map s (Transitions s a)	-- All the DFA transitions
      , finals      :: Set s			-- Accepting states
      } deriving Show

instance (Ord a, Ord s) => AcceptFA (DFA s a) where
  type StateType (DFA s a) = s
  type InputType (DFA s a) = a

  initial    = start
  step f x q = M.lookup q (transitions f) >>= \qts ->
                 M.lookup (Symbol x) qts <|> M.lookup Default qts
  final f q  = q `S.member` finals f

-- A minimal DFA containing a single non-accepting initial state q0.
unit :: s -> DFA s a
unit q0 = DFA q0 M.empty S.empty

trans :: (Ord a, Ord s) => Transition s a -> DFA s a -> DFA s a
trans (q,i,q') (DFA q0 ts fs) = DFA q0 (insert ts) fs
  where insert = M.insertWith M.union q (M.singleton i q')

final :: (Ord a, Ord s) => s -> DFA s a -> DFA s a
final q (DFA q0 ts fs) = DFA q0 ts (S.insert q fs)

states :: (Ord a, Ord s) => DFA s a -> Set s
states (DFA q0 ts fs) = S.unions [S.singleton q0, states' ts, fs]
  where states'      = M.foldrWithKey add S.empty
        add q qts qs = M.foldrWithKey (const S.insert) (S.insert q qs) qts

size :: (Ord a, Ord s) => DFA s a -> Int
size = S.size . states
