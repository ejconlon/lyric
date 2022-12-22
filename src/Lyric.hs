{-# LANGUAGE TemplateHaskell #-}

module Lyric where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State.Strict (MonadState, State, gets, runState, modify')
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

data Err =
    ErrStepKontNonValue
  | ErrTodo
  deriving stock (Eq, Ord, Show)

instance Exception Err

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

type Heap = Map TmUniq Val

initHeap :: Heap
initHeap = Map.empty

type Ctx = Map TmVar TmUniq

initCtx :: Ctx
initCtx = Map.empty

type Union = UnionMap TmUniq Val

initUnion :: Union
initUnion = UM.empty

data Kont =
    KontTop
  | KontOne !(Seq Exp) Kont
  | KontAll !(Seq Val) !(Seq Exp) Kont
  deriving stock (Eq, Ord, Show)

makeBaseFunctor ''Kont
deriving instance Eq r => Eq (KontF r)
deriving instance Ord r => Ord (KontF r)
deriving instance Show r => Show (KontF r)

data Frame = Frame
  { frCtx :: !Ctx
  , frUnion :: !Union
  , frParent :: !Stack
  } deriving stock (Eq, Show)

initFrame :: Frame
initFrame = Frame initCtx initUnion initStack

newtype Stack = Stack { unStack :: KontF Frame }
  deriving stock (Show)
  deriving newtype (Eq)

initStack :: Stack
initStack = Stack KontTopF

data St = St
  { stUniq :: !TmUniq
  , stFocus :: !Exp
  , stHeap :: !Heap
  , stCtx :: !Ctx
  , stUnion :: !Union
  , stStack :: !Stack
  } deriving stock (Eq, Show)

initSt :: Exp -> St
initSt focus = St 0 focus initHeap initCtx initUnion initStack

newtype M a = M { unM :: ExceptT Err (State St) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Err, MonadState St)

runM :: M a -> St -> (Either Err a, St)
runM = runState . runExceptT . unM

data FocusRes =
    FocusResRet
  | FocusResCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data KontRes =
    KontResCont
  | KontResDone
  deriving stock (Eq, Ord, Show, Enum, Bounded)

stepFocus :: M FocusRes
stepFocus = do
  focus <- gets stFocus
  case focus of
    ExpVal _ -> pure FocusResRet
    ExpFail -> pure FocusResRet
    _ -> throwError ErrTodo

readVal :: M (Maybe Val)
readVal = do
  focus <- gets stFocus
  case focus of
    ExpVal v -> pure (Just v)
    ExpFail -> pure Nothing
    _ -> throwError ErrStepKontNonValue

stepKont :: M KontRes
stepKont = do
  Stack kontf <- gets stStack
  case kontf of
    KontTopF -> pure KontResDone
    KontOneF es fr -> do
      mv <- readVal
      error "TODO"
      -- case mv of
      --   Nothing ->
      --     case es of
      --       Empty ->
      --         error "TODO"
      --       e :<| es' -> do
      --         error "TODO"
      -- pure KontResCont
    KontAllF vs es fr -> do
      error "TODO"
      -- let vs' = vs :|> v
      -- case es of
      --   Empty -> do
      --     -- let e = ExpVal (ValTup (fmap ExpVal vs'))
      --     -- modify' (\st -> st { stFocus = e, stKont = k })
      --     error "TODO"
      --   e :<| es' -> do
      --     -- let k' = KontAll vs' es' k
      --     -- modify' (\st -> st { stFocus = e, stKont = k' })
      --     error "TODO"
      -- pure KontResCont

step :: M KontRes
step = do
  resFocus <- stepFocus
  case resFocus of
    FocusResCont -> pure KontResCont
    FocusResRet -> stepKont

multiStep :: M ()
multiStep = do
  res <- step
  case res of
    KontResCont -> multiStep
    KontResDone -> pure ()
