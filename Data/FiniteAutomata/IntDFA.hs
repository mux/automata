module Data.FiniteAutomata.IntDFA where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- Like DFA but using s = Int in order to use the optimized
-- IntMap and IntSet datatypes.

type State = Int

data Input a = Symbol a
             | Default
  deriving (Eq,Ord,Show)

type Transition a  = (State, Input a, State)
type Transitions a = Map (Input a) State

data IntDFA a =
  IntDFA { start       :: State
         , transitions :: IntMap (Transitions a)
         , finals      :: IntSet
         } deriving Show

unit :: State -> IntDFA a
unit q0 = IntDFA q0 IM.empty IS.empty

trans :: Ord a => Transition a -> IntDFA a -> IntDFA a
trans (q,i,q') (IntDFA q0 ts fs) = IntDFA q0 (insert ts) fs
  where insert = IM.insertWith M.union q (M.singleton i q')

final :: Ord a => State -> IntDFA a -> IntDFA a
final q (IntDFA q0 ts fs) = IntDFA q0 ts (IS.insert q fs)

states :: Ord a => IntDFA a -> IntSet
states (IntDFA q0 ts fs) = IS.unions [IS.singleton q0, states' ts, fs]
  where states'      = IM.foldWithKey add IS.empty
        add q qts qs = M.foldrWithKey (const IS.insert) (IS.insert q qs) qts

size :: Ord a => IntDFA a -> Int
size = IS.size . states

step :: Ord a => IntDFA a -> a -> State -> Maybe State
step (IntDFA _ ts _) x q = IM.lookup q ts >>= \qts ->
                             M.lookup (Symbol x) qts <|> M.lookup Default qts

accept :: Ord a => IntDFA a -> [a] -> Bool
accept d@(IntDFA q0 _ fs) = go q0
  where go q []     = q `IS.member` fs
        go q (x:xs) = maybe False (`go` xs) (step d x q)
