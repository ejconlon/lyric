{-# LANGUAGE TemplateHaskell #-}

module Lyric where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.State.Strict (MonadState, State, runState)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq (..))
import Data.String (IsString)
import Data.Text (Text)

newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Op = OpGt | OpAdd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Exp =
    ExpTup !(Seq Exp)
  | ExpLam !TmVar Exp
  | ExpInt !Int
  | ExpOp !Op
  | ExpApp Exp Exp
  | ExpSeq Exp Exp
  | ExpExists !TmVar Exp
  | ExpFail
  | ExpAlt Exp Exp
  | ExpOne Exp
  | ExpAll Exp
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Exp
deriving instance Eq r => Eq (ExpF r)
deriving instance Ord r => Ord (ExpF r)
deriving instance Show r => Show (ExpF r)

data Err = ErrFoo
  deriving stock (Eq, Ord, Show)

instance Exception Err

data St = St
  {
  } deriving stock (Eq, Ord, Show)

newtype M a = M { unM :: ExceptT Err (State St) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Err, MonadState St)

runM :: M a -> St -> (Either Err a, St)
runM = runState . runExceptT . unM

step :: M ()
step = pure ()
