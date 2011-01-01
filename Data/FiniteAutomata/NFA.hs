module Data.FiniteAutomata.NFA where

import Data.Foldable
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.FiniteAutomata.DFA (DFA)
import qualified Data.FiniteAutomata.DFA as D

data Input a = Symbol a
             | Epsilon
             | Any		-- Not part of the theory, but convenient
  deriving (Eq,Ord,Show)

type Transition s a  = (s, Input a, s)
type Transitions s a = Map (Input a) (Set s)

data NFA s a =
  NFA { start       :: s
      , transitions :: Map s (Transitions s a)
      , finals      :: Set s
      } deriving Show

-- A minimal NFA containing a single non-accepting initial state q0.
unit :: s -> NFA s a
unit q0 = NFA q0 M.empty S.empty

trans :: (Ord a, Ord s) => Transition s a -> NFA s a -> NFA s a
trans (q,i,q') (NFA q0 ts fs) = NFA q0 (insert ts) fs
  where insert = M.insertWith (M.unionWith S.union) q
                              (M.singleton i (S.singleton q'))

final :: (Ord a, Ord s) => s -> NFA s a -> NFA s a
final q (NFA q0 ts fs) = NFA q0 ts (S.insert q fs)

states :: (Ord a, Ord s) => NFA s a -> Set s
states (NFA q0 ts fs) = S.unions [S.singleton q0, states' ts, fs]
  where states'      = M.foldrWithKey add S.empty
        add q qts qs = M.foldrWithKey (const S.union) (S.insert q qs) qts

size :: (Ord a, Ord s) => NFA s a -> Int
size = S.size . states

stepi1 :: (Ord a, Ord s) => NFA s a -> Input a -> s -> Set s
stepi1 (NFA _ ts _) i = maybe S.empty next . (`M.lookup` ts)
  where next = fromMaybe S.empty . M.lookup i

stepi :: (Ord a, Ord s) => NFA s a -> Input a -> Set s -> Set s
stepi nfa i = foldMap (stepi1 nfa i)

step :: (Ord a, Ord s) => NFA s a -> a -> Set s -> Set s
step nfa x = go . eclosure nfa
  where go qs = stepi nfa (Symbol x) qs `S.union` stepi nfa Any qs

accept :: (Ord a, Ord s) => NFA s a -> [a] -> Bool
accept nfa@(NFA q0 _ fs) = go (S.singleton q0)
  where go qs [] = not . S.null $ qs `S.intersection` fs
        go qs (x:xs)
          | S.null next = False
          | otherwise   = go next xs
          where next = step nfa x qs

-- Compute the epsilon-closure of a set of states.
eclosure :: (Ord a, Ord s) => NFA s a -> Set s -> Set s
eclosure nfa qs = go qs qs
  where go todo eqs
          | S.null todo = eqs
          | otherwise   = go (next `S.difference` eqs)  (eqs `S.union` next)
          where next = stepi nfa Epsilon todo

-- Convert the NFA into a DFA using the powerset-construction algorithm.
determinize :: (Ord a, Ord s) => NFA s a -> DFA (Set s) a
determinize nfa@(NFA q0 ts fs) = go (S.singleton dfaq0) S.empty (D.unit dfaq0)
  where
   dfaq0            = eclosure nfa (S.singleton q0)
   go todo done dfa = maybe dfa build (S.minView todo)
    where
     build (q, todo') = go todo'' done' $
                          if qfinal then D.final q dfa' else dfa'
      where
       done'         = S.insert q done
       -- Analyze a DFA state (set of states): merge all the transitions and
       -- check if any of the state is final in the NFA, in one pass.
       (qts, qfinal) = S.fold analyze (M.empty, False) q
        where
         analyze q' (qts',f) = (merge q' qts', f || q' `S.member` fs)
          where
            merge = M.unionWith S.union . fromMaybe M.empty . (`M.lookup` ts)

       -- Create transitions in the DFA corresponding to the merged
       -- transitions and add next states to handle in the todo set.
       (dfa', todo'') = M.foldrWithKey add (dfa, todo') $ qts
       anyqs          = fromMaybe S.empty $ M.lookup Any qts

       add i qs (dfa, next) = (add' i qs' dfa, next')
         where next' = next `S.union` (S.singleton qs' `S.difference` done')
               qs'   = eclosure nfa qs

       add' (Symbol x) qs = D.trans (q, D.Symbol x, qs `S.union` anyqs)
       add' Any        qs = D.trans (q, D.Default, qs)
       add' _          _  = id
