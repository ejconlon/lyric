-- | A map of equivalence class to some class value.
-- The map keys will only contain equivalence class roots.
module Lyric.UnionMap
  ( UnionMap
  , unionFind
  , valueMap
  , fromMap
  , empty
  , size
  , insert
  , stateInsertL
  , find
  , stateFindL
  , MergeFun
  , stateMergeL
  ) where

import Control.Lens (Lens')
import Control.Monad.State.Strict (MonadState)
import Data.Coerce (Coercible)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import Lyric.Lenses (execStateLens, runStateLens)
import Lyric.UnionFind (MergeRes (..), UnionFind)
import qualified Lyric.UnionFind as UF

data UnionMap k v = UnionMap
  { unionFind :: !(UnionFind k)
  , valueMap :: !(IntLikeMap k v)
  } deriving stock (Eq, Show)

fromMap :: Coercible k Int => IntLikeMap k v -> UnionMap k v
fromMap m = UnionMap (UF.fromSet (ILM.keysSet m)) m

empty :: UnionMap k v
empty = UnionMap UF.empty ILM.empty

size :: UnionMap k v -> Int
size = UF.size . unionFind

insert :: Coercible k Int => k -> v -> UnionMap k v -> UnionMap k v
insert k v (UnionMap uf m) = UnionMap (UF.insert k uf) (ILM.insert k v m)

stateInsertL :: (Coercible k Int, MonadState s m) => Lens' s (UnionMap k v) -> k -> v -> m ()
stateInsertL l k v = execStateLens l (pure . insert k v)

find :: (Eq k, Coercible k Int) => k -> UnionMap k v -> (Maybe (k, v), UnionMap k v)
find k (UnionMap uf m) =
  let (mx, uf') = UF.find k uf
      u' = UnionMap uf' m
      mp = fmap (\x -> (x, ILM.partialLookup x m)) mx
  in (mp, u')

stateFindL :: (Eq k, Coercible k Int, MonadState s m) => Lens' s (UnionMap k v) -> k -> m (Maybe (k, v))
stateFindL l k = runStateLens l (pure . find k)

-- Must be symmetric, reflexive, etc
type MergeFun m v = v -> v -> m v

-- Note: this is written carefully to allow the merge function to update the union map state so recursive
-- merges can be applied. Be very careful: it's not going to yield the correct results if recursive calls
-- merge keys that are already waiting for their values to be merged... Essentially, this means that your
-- graph should not have cycles.
stateMergeL :: (Ord k, Coercible k Int, MonadState s m) => Lens' s (UnionMap k v) -> MergeFun m v -> k -> k -> m (MergeRes k)
stateMergeL l f a b = do
  -- Merge classes in the union find
  -- and write the result immediately
  (res, mz) <- runStateLens l $ \(UnionMap uf m) ->
    let (res, uf') = UF.merge a b uf
        mz = case res of
          MergeResChanged knew kold ->
            let vnew = ILM.partialLookup knew m
                vold = ILM.partialLookup kold m
            in Just (knew, vnew, kold, vold)
          _ -> Nothing
    in pure ((res, mz), UnionMap uf' m)
  -- If it was indeed a new merge, combine their values
  case mz of
    Just (knew, vnew, kold, vold) -> do
      -- Perform the merge effect (Which may write the UM)
      vmerge <- f vnew vold
      -- Lookup the UM to finish the merge
      execStateLens l $ \(UnionMap uf m') ->
        let m'' = ILM.insert knew vmerge (ILM.delete kold m')
        in pure (UnionMap uf m'')
    _ -> pure ()
  pure res
