{-# LANGUAGE TemplateHaskell #-}

module Lyric where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.State.Strict (MonadState, State, gets, runState)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import Data.String (IsString)
import Data.Text (Text)
import Lyric.UnionMap (UnionMap)
import qualified Lyric.UnionMap as UM

newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Op = OpGt | OpAdd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Val =
    ValVar !TmVar
  | ValTup !(Seq Exp)
  | ValLam !TmVar Exp
  | ValInt !Int
  | ValOp !Op
  deriving stock (Eq, Ord, Show)

data Exp =
    ExpVal Val
  | ExpApp Exp Exp
  | ExpSeq Exp Exp
  | ExpExists !TmVar Exp
  | ExpFail
  | ExpAlt Exp Exp
  | ExpOne Exp
  | ExpAll Exp
  | ExpUnify !TmVar Exp
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Exp
deriving instance Eq r => Eq (ExpF r)
deriving instance Ord r => Ord (ExpF r)
deriving instance Show r => Show (ExpF r)

data Err = ErrFoo
  deriving stock (Eq, Ord, Show)

instance Exception Err

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

type VarEnv = Map TmVar TmUniq

type VarUnion = UnionMap TmUniq Val

data Kont = KontTop
  deriving stock (Eq, Ord, Show)

data St = St
  { stUniq :: !TmUniq
  , stFocus :: !Exp
  , stVarEnv :: !VarEnv
  , stVarUnion :: !VarUnion
  , stKont :: !Kont
  } deriving stock (Eq, Show)

initSt :: Exp -> St
initSt focus = St 0 focus Map.empty UM.empty KontTop

newtype M a = M { unM :: ExceptT Err (State St) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Err, MonadState St)

runM :: M a -> St -> (Either Err a, St)
runM = runState . runExceptT . unM

data Res = ResCont | ResDone
  deriving stock (Eq, Ord, Show, Enum, Bounded)

stepFocus :: M (Maybe Res)
stepFocus = do
  focus <- gets stFocus
  case focus of
    -- TODO
    _ -> pure Nothing

stepKont :: M (Maybe Res)
stepKont = do
  kont <- gets stKont
  case kont of
    -- TODO
    _ -> pure Nothing

step :: M Res
step = do
  resFocus <- stepFocus
  case resFocus of
    Nothing -> do
      resKont <- stepKont
      case resKont of
        Nothing -> pure ResDone
        Just res -> pure res
    Just res -> pure res

multiStep :: M ()
multiStep = do
  res <- step
  case res of
    ResCont -> multiStep
    ResDone -> pure ()
