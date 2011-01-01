module Data.FiniteAutomata.DFA where

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

type Transition s a  = (s, a, s)	-- One single transition
type Transitions s a = Map a s		-- Outgoing transitions of a state

data DFA s a =
  DFA { start       :: s			-- Initial state
      , transitions :: Map s (Transitions s a)	-- All the DFA transitions
      , finals      :: Set s			-- Accepting states
      } deriving Show

-- A minimal DFA containing a single non-accepting initial state q0.
unit :: s -> DFA s a
unit q0 = DFA q0 M.empty S.empty

trans :: (Ord a, Ord s) => Transition s a -> DFA s a -> DFA s a
trans (q,x,q') (DFA q0 ts fs) = DFA q0 (insert ts) fs
  where insert = M.insertWith M.union q (M.singleton x q')

final :: (Ord a, Ord s) => s -> DFA s a -> DFA s a
final q (DFA q0 ts fs) = DFA q0 ts (S.insert q fs)

states :: (Ord a, Ord s) => DFA s a -> Set s
states (DFA q0 ts fs) = S.unions [S.singleton q0, states' ts, fs]
  where states'      = M.foldrWithKey add S.empty
        add q qts qs = M.foldrWithKey (const S.insert) (S.insert q qs) qts

size :: (Ord a, Ord s) => DFA s a -> Int
size = S.size . states

step :: (Ord a, Ord s) => DFA s a -> a -> s -> Maybe s
step (DFA _ ts _) x q = M.lookup q ts >>= M.lookup x

accept :: (Ord a, Ord s) => DFA s a -> [a] -> Bool
accept d@(DFA q0 _ fs) = go q0
  where go q []     = q `S.member` fs
        go q (x:xs) = maybe False (`go` xs) (step d x q)
