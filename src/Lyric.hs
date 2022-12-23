{-# LANGUAGE TemplateHaskell #-}

module Lyric where

import Control.Exception (Exception)
import Control.Lens (Lens')
import Control.Lens.TH (makeLensesFor)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState, State, gets, modify', runState)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Lyric.Lenses (runStateLens)
import Lyric.UnionFind (MergeRes (..))
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
  deriving stock (Eq, Ord, Show)

-- valCtx :: Val -> Ctx
-- valCtx = \case
--   ValLam ctx _ _ -> ctx
--   _ -> Map.empty

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
  | MergeErrMismatch
  deriving stock (Eq, Ord, Show)

instance Exception MergeErr

data Trail =
    TrailStart !TmUniq !TmUniq
  | TrailTup !Int !Trail
  deriving stock (Eq, Ord, Show)

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

data Kont =
    KontTop
  | KontOne !(Seq Exp) !Env Kont
  | KontAll !(Seq Val) !(Seq Exp) !Env Kont
  deriving stock (Eq, Show)

makeBaseFunctor ''Kont
deriving instance Eq r => Eq (KontF r)
deriving instance Show r => Show (KontF r)

-- data Frame = Frame
--   { frCtx :: !Ctx
--   , frUnion :: !Union
--   , frParent :: !Stack
--   } deriving stock (Eq, Show)

-- initFrame :: Frame
-- initFrame = Frame initCtx initUnion initStack

-- newtype Stack = Stack { unStack :: KontF Frame }
--   deriving stock (Show)
--   deriving newtype (Eq)

-- initStack :: Stack
-- initStack = Stack KontTopF

data Focus =
    FocusRed !Exp
  | FocusRet !(Maybe Val)
  deriving stock (Eq, Ord, Show)

data St = St
  { stUniq :: !TmUniq
  , stFocus :: !Focus
  , stEnv :: !Env
  , stKont :: !Kont
  } deriving stock (Eq, Show)

makeLensesFor [("stEnv", "stEnvL")] ''St

stUnionL :: Lens' St Union
stUnionL = stEnvL . envUnionL

initSt :: Exp -> St
initSt e = St 0 (FocusRed e) initEnv KontTop

newtype M a = M { unM :: ExceptT Err (State St) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Err, MonadState St)

runM :: M a -> St -> (Either Err a, St)
runM = runState . runExceptT . unM

setFocus :: Focus -> M ()
setFocus e = modify' (\st -> st { stFocus = e })

setEnv :: Env -> M ()
setEnv env = modify' (\st -> st { stEnv = env })

setKont :: Kont -> M ()
setKont k = modify' (\st -> st { stKont = k })

data StepRes =
    StepResCont
  | StepResDone
  deriving stock (Eq, Ord, Show, Enum, Bounded)

stepFocus :: Exp -> M ()
stepFocus = \case
    ExpFail -> do
      setFocus (FocusRet Nothing)
    ExpInt k -> do
      setFocus (FocusRet (Just (ValInt k)))
    _ -> throwError ErrTodo

stepKont :: Maybe Val -> M StepRes
stepKont mv = do
  kont <- gets stKont
  case kont of
    KontTop -> pure StepResDone
    KontOne es ienv k -> do
      case mv of
        Nothing ->
          case es of
            Empty -> do
              setEnv ienv
              setKont k
            e :<| es' -> do
              setFocus (FocusRed e)
              setEnv ienv
              setKont (KontOne es' ienv k)
        Just _ -> do
          setKont k
      pure StepResCont
    KontAll vs es ienv k -> do
      let vs' = maybe vs (vs :|>) mv
      case es of
        Empty -> do
          -- let e = ExpVal (ValTup (fmap ExpVal vs'))
          -- setFocus e
          error "TODO"
        e :<| es' -> do
          error "TODO"
      --     -- let k' = KontAll vs' es' k
      --     -- modify' (\st -> st { stFocus = e, stKont = k' })
      --     error "TODO"
      pure StepResCont

step :: M StepRes
step = do
  focus <- gets stFocus
  case focus of
    FocusRed e -> StepResCont <$ stepFocus e
    FocusRet mv -> stepKont mv

multiStep :: M ()
multiStep = do
  res <- step
  case res of
    StepResCont -> multiStep
    StepResDone -> pure ()

newtype N a = N { unN :: ReaderT Trail (ExceptT TrailErr (State Union)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Trail, MonadState Union, MonadError TrailErr)

runN :: N a -> Trail -> Union -> (Either TrailErr a, Union)
runN n t = runState (runExceptT (runReaderT (unN n) t))

embedN :: N a -> Trail -> M a
embedN n t = runStateLens stUnionL $ \u -> do
  let (ea, u') = runN n t u
  case ea of
    Left e -> throwError (ErrMerge e)
    Right a -> pure (a, u')

throwMergeErr :: MergeErr -> N a
throwMergeErr me = do
  t <- ask
  throwError (TrailErr t me)

tupZip :: Seq Val -> Seq Val -> N (Seq Val)
tupZip = go 0 where
  go _ Empty _ = pure Empty
  go _ _ Empty = pure Empty
  go !i (a :<| as) (b :<| bs) = do
    c <- local (TrailTup i) (mergeVal a b)
    cs <- go (i + 1) as bs
    pure (c :<| cs)

mergeVal :: Val -> Val -> N Val
mergeVal x y =
  case (x, y) of
    (_, ValVar _) -> pure x
    (ValVar _, _) -> pure y
    (ValTup xvs, ValTup yvs) -> do
      let xlen = Seq.length xvs
          ylen = Seq.length yvs
      if xlen == ylen
        then fmap ValTup (tupZip xvs yvs)
        else throwMergeErr (MergeErrTupLen xlen ylen)
    (ValLam {}, ValLam {}) -> throwMergeErr MergeErrLam
    (ValInt xk, ValInt yk) ->
      if xk == yk
        then pure x
        else throwMergeErr (MergeErrInt xk yk)
    (ValOp xo, ValOp yo) ->
      if xo == yo
        then pure x
        else throwMergeErr (MergeErrOp xo yo)
    _ -> throwMergeErr MergeErrMismatch

mergeUniq :: TmUniq -> TmUniq -> M TmUniq
mergeUniq a b = do
  res <- embedN (UM.stateMergeL id mergeVal a b) (TrailStart a b)
  case res of
    MergeResMissing x -> throwError (ErrMissing x)
    MergeResUnchanged x -> pure x
    MergeResChanged x _ -> pure x

expApp :: Exp -> [Exp] -> Exp
expApp a = \case
  [] -> a
  b:bs -> expApp (ExpApp a b) bs

expAlts :: [Exp] -> Exp
expAlts = \case
  [] -> ExpFail
  [b] -> b
  b:bs -> ExpAlt b (expAlts bs)

testExp :: Exp
testExp = expApp (ExpOp OpAdd) [ExpInt 1, ExpInt 2]

evalExp :: Exp -> Either (Err, St) Focus
evalExp ex =
  let (eu, st) = runM multiStep (initSt ex)
  in case eu of
    Left er -> Left (er, st)
    Right _ -> Right (stFocus st)
