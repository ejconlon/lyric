-- | A very simple union-find to track equivalence classes.
-- Implemented as a map from a node to a parent node.
-- (If a node is its own parent, then it is the root of the class.)
-- Uses path compression to update pointers as demanded.
module Lyric.UnionFind
  ( UnionFind
  , fromSet
  , empty
  , size
  , members
  , stateMembers
  , insert
  , stateInsert
  , find
  , stateFind
  , MergeRes (..)
  , merge
  , stateMerge
  ) where

import Control.Lens (Lens')
import Control.Monad.State.Strict (MonadState)
import Data.Coerce (Coercible)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Lyric.Lenses (execStateLens, runStateLens)

-- private ctor
data UnionFind x = UnionFind
  { ufSize :: !Int
  , ufParents :: !(IntLikeMap x x)
  } deriving stock (Eq, Show)

fromSet :: Coercible x Int => IntLikeSet x -> UnionFind x
fromSet s = UnionFind (ILS.size s) (ILM.fromList (fmap (\x -> (x, x)) (ILS.toList s)))

empty :: UnionFind x
empty = UnionFind 0 ILM.empty

size :: UnionFind x -> Int
size = ufSize

members :: (Eq x, Coercible x Int) => UnionFind x -> (IntLikeMap x (IntLikeSet x), UnionFind x)
members u@(UnionFind _ p) = foldr go (ILM.empty, u) (ILM.keys p) where
  go x1 (m, v) =
    let (x2, v') = findRoot v x1
        m' = ILM.insert x2 (maybe (ILS.singleton x1) (ILS.insert x1) (ILM.lookup x2 m)) m
    in (m', v')

stateMembers :: (Eq x, Coercible x Int, MonadState s m) => Lens' s (UnionFind x) -> m (IntLikeMap x (IntLikeSet x))
stateMembers l = runStateLens l (pure . members)

insert :: Coercible x Int => x -> UnionFind x -> UnionFind x
insert x u@(UnionFind z p) =
  if ILM.member x p
    then u
    else UnionFind (z + 1) (ILM.insert x x p)

stateInsert :: (Coercible x Int, MonadState s m) => Lens' s (UnionFind x) -> x -> m ()
stateInsert l x = execStateLens l (pure . insert x)

-- private
findRootAcc :: (Eq x, Coercible x Int) => IntLikeMap x x -> [x] -> x -> ([x], x)
findRootAcc p acc x1 =
    let x2 = ILM.partialLookup x1 p
    in if x1 == x2
      then (acc, x2)
      else findRootAcc p (x1:acc) x2

-- private
findRoot :: (Eq x, Coercible x Int) => UnionFind x -> x -> (x, UnionFind x)
findRoot u@(UnionFind z p) x1 =
  let (acc, x2) = findRootAcc p [] x1
      u' = case acc of
            [] -> u
            _ -> let p' = foldr (`ILM.insert` x2) p acc
                 in UnionFind z p'
  in (x2, u')

find :: (Eq x, Coercible x Int) => x -> UnionFind x -> (Maybe x, UnionFind x)
find a u@(UnionFind _ p) = maybe (Nothing, u) (\x -> let (y, v) = findRoot u x in (Just y, v)) (ILM.lookup a p)

stateFind :: (Eq x, Coercible x Int, MonadState s m) => Lens' s (UnionFind x) -> x -> m (Maybe x)
stateFind l x = runStateLens l (pure . find x)

data MergeRes x =
    MergeResMissing !x
  -- ^ Key not present in UF - must be added before merging
  | MergeResUnchanged !x
  -- ^ Root unchanged, though merge may path-compress in the process
  | MergeResChanged !x !x
  -- ^ (New root class, old root class)
  deriving stock (Eq, Ord, Show)

merge :: (Ord x, Coercible x Int) => x -> x -> UnionFind x -> (MergeRes x, UnionFind x)
merge i j u@(UnionFind z p) = guardMerge where
  guardMerge =
    if ILM.member i p
      then if ILM.member j p
        then doMerge i j
        else (MergeResMissing j, u)
      else (MergeResMissing i, u)
  doMerge ix1 jx1 =
    let (iacc, ix2) = findRootAcc p [] ix1
        (acc, jx2) = findRootAcc p iacc jx1
    in if ix2 == jx2
      then
        let u' = case acc of
                [] -> u
                _ -> let p' = foldr (`ILM.insert` ix2) p acc
                    in UnionFind z p'
        in (MergeResUnchanged ix2, u')
      else
        let (kacc, knew, kold) =
              if ix2 < jx2
                then (if jx1 == jx2 then jx1:acc else jx2:jx1:acc, ix2, jx2)
                else (if ix1 == ix2 then ix1:acc else ix2:ix1:acc, jx2, ix2)
            p' = foldr (`ILM.insert` knew) p kacc
            u' = UnionFind (z - 1) p'
        in (MergeResChanged knew kold, u')

stateMerge :: (Ord x, Coercible x Int, MonadState s m) => Lens' s (UnionFind x) -> x -> x -> m (MergeRes x)
stateMerge l i j = runStateLens l (pure . merge i j)
