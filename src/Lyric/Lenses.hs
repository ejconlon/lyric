module Lyric.Lenses
  ( runStateLens
  , execStateLens
  ) where

import Control.Lens (Lens')
import Control.Lens.Combinators (set, view)
import Control.Monad.State.Strict (MonadState, gets, modify')

runStateLens :: MonadState s m => Lens' s a -> (a -> m (b, a)) -> m b
runStateLens l f = do
  a <- gets (view l)
  (b, a') <- f a
  modify' (set l a')
  pure b

execStateLens :: MonadState s m => Lens' s a -> (a -> m a) -> m ()
execStateLens l f = do
  a <- gets (view l)
  a' <- f a
  modify' (set l a')
