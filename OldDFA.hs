module DFA where

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data DFA a =
  DFA { start  :: DFAStateRef
      , states :: IntMap (DFAState a)
      }

type DFAStateRef = Int

type DFAEdge a = (a, DFAStateRef)

data DFAState a =
  DFAState { edges :: Map a DFAStateRef
           , final :: !Bool
           }

type DFAStateRepr a = (DFAStateRef, Bool, [(a, DFAStateRef)])

idfa :: Ord a => DFAStateRef -> [DFAStateRepr a] -> DFA a
idfa q0 qs = DFA { start  = q0
                  , states = IM.fromList (convert qs) }
  where convert = map (\(i,f,ts) -> (i, DFAState (M.fromList ts) f))

accept :: Ord a => DFA a -> [a] -> Bool
accept (DFA { start = q0, states = qs }) = go q0
  where go ref word =
          let q = qs IM.! ref in
          case word of
             []     -> final q
             (x:xs) -> maybe False (`go` xs) (M.lookup x (edges q))

dfa :: DFA Char
dfa = idfa 1 [ (1, True,  [('0', 2), ('1', 1)])
             , (2, False, [('0', 1), ('1', 2)])
             ]
