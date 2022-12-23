{-# LANGUAGE TemplateHaskell #-}

module Lyric.Core where

import Control.Exception (Exception)
import Control.Lens (Lens')
import Control.Lens.TH (makeLensesFor)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Lyric.UnionMap (UnionMap)
import qualified Lyric.UnionMap as UM

newtype Index = Index { unTmIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Op = OpGt | OpAdd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Exp =
    ExpVar !TmVar
  | ExpTup !(Seq Exp)
  | ExpLam !TmVar !Exp
  | ExpInt !Int
  | ExpOp !Op
  | ExpApp Exp Exp
  | ExpSeq Exp Exp
  | ExpExists !TmVar Exp
  | ExpFail
  | ExpAlt Exp Exp
  | ExpOne Exp
  | ExpAll Exp
  | ExpUnify !TmVar Exp
  | ExpLet !TmVar Exp Exp
  deriving stock (Eq, Ord, Show)

instance IsString Exp where
  fromString = ExpVar . fromString

makeBaseFunctor ''Exp
deriving instance Eq r => Eq (ExpF r)
deriving instance Ord r => Ord (ExpF r)
deriving instance Show r => Show (ExpF r)

type Ctx = Map TmVar TmUniq

initCtx :: Ctx
initCtx = Map.empty

data Val =
    ValVar !TmUniq
  | ValTup !(Seq Val)
  | ValLam !Ctx !TmVar !Exp
  | ValInt !Int
  | ValOp !Op
  | ValPart !Int !Val !(Seq Val)
  deriving stock (Eq, Ord, Show)

-- type Trim = IntLikeSet TmUniq

-- data Slot = Slot
--   { slotTrim :: !Trim
--   , slotVal :: !Val
--   } deriving stock (Eq, Show)

type Union = UnionMap TmUniq Val

initUnion :: Union
initUnion = UM.empty

data MergeErr =
    MergeErrTupLen !Int !Int
  | MergeErrLam
  | MergeErrInt !Int !Int
  | MergeErrOp !Op !Op
  | MergeErrPart
  | MergeErrMismatch
  deriving stock (Eq, Ord, Show)

instance Exception MergeErr

data Trail =
    TrailStart !TmUniq !TmUniq
  | TrailTup !Int !Trail
  | TrailPartHead !Trail
  | TrailPartTail !Int !Trail
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Trail
deriving instance Eq r => Eq (TrailF r)
deriving instance Ord r => Ord (TrailF r)
deriving instance Show r => Show (TrailF r)

data TrailErr = TrailErr !Trail !MergeErr
  deriving stock (Eq, Ord, Show)

instance Exception TrailErr

data Err =
    ErrMerge !TrailErr
  | ErrMissing !TmUniq
  | ErrTodo
  deriving stock (Eq, Ord, Show)

instance Exception Err

data Env = Env
  { envCtx :: !Ctx
  , envUnion :: !Union
  } deriving stock (Eq, Show)

makeLensesFor [("envUnion", "envUnionL")] ''Env

initEnv :: Env
initEnv = Env initCtx initUnion

data RedKont =
    RedKontTop
  | RedKontAlt !Env !Exp RedKont
  | RedKontAppFirst !Exp RedKont
  | RedKontAppSecond !Val RedKont
  deriving stock (Eq, Show)

makeBaseFunctor ''RedKont
deriving instance Eq r => Eq (RedKontF r)
deriving instance Show r => Show (RedKontF r)

data CtlKont =
    CtlKontTop
  | CtlKontOne !RedKont CtlKont
  | CtlKontAll !(Seq Val) !RedKont CtlKont
  deriving stock (Eq, Show)

ctlRedKont :: CtlKont -> RedKont
ctlRedKont = \case
  CtlKontTop -> RedKontTop
  CtlKontOne k _ -> k
  CtlKontAll _ k _ -> k

makeBaseFunctor ''CtlKont
deriving instance Eq r => Eq (CtlKontF r)
deriving instance Show r => Show (CtlKontF r)

data RetVal =
    RetValFail
  | RetValPure !Val
  deriving stock (Eq, Ord, Show)

data Focus =
    FocusRed !Exp
  | FocusRet !RetVal
  | FocusCtl !RetVal
  | FocusAlt !Env !Exp !RedKont !Val
  deriving stock (Eq, Show)

data St = St
  { stUniq :: !TmUniq
  , stFocus :: !Focus
  , stEnv :: !Env
  , stCtlKont :: !CtlKont
  } deriving stock (Eq, Show)

makeLensesFor [("stEnv", "stEnvL")] ''St

stUnionL :: Lens' St Union
stUnionL = stEnvL . envUnionL

initSt :: Exp -> St
initSt e = St 0 (FocusRed e) initEnv CtlKontTop
