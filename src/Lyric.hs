module Lyric where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), State, gets, modify', runState)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Lyric.Core (Alt (..), CtlKont (..), Ctx, Env, Err (..), Exp (..), Focus (..), Fun (..), MergeErr (..), Op (..),
                   RedKont (..), RetVal (..), St (..), TmUniq (..), TmVar, Trail (..), TrailErr (..), Union, Val (..),
                   ctlAddAlt, ctlRedKont, matchFun, stUnionL)
import Lyric.Lenses (runStateLens)
import Lyric.UnionFind (MergeRes (..))
import qualified Lyric.UnionMap as UM

newtype M a = M { unM :: ExceptT Err (State St) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Err, MonadState St)

runM :: M a -> St -> (Either Err a, St)
runM = runState . runExceptT . unM

todo :: String -> M a
todo = throwError . ErrTodo

setFocus :: Focus -> M ()
setFocus e = modify' (\st -> st { stFocus = e })

setEnv :: Env -> M ()
setEnv env = modify' (\st -> st { stEnv = env })

setCtlKont :: CtlKont -> M ()
setCtlKont k = modify' (\st -> st { stCtlKont = k })

modifyCtlKont :: (CtlKont -> CtlKont) -> M ()
modifyCtlKont f = modify' (\st -> st { stCtlKont = f (stCtlKont st)})

setRedKont :: RedKont -> M ()
setRedKont = modifyRedKont . const

modifyRedKont :: (RedKont -> RedKont) -> M ()
modifyRedKont f = modify' $ \st ->
  st { stCtlKont =
    case stCtlKont st of
      CtlKontTop -> CtlKontOne (f RedKontTop) Empty CtlKontTop
      CtlKontOne k as j -> CtlKontOne (f k) as j
      CtlKontAll k as vs j -> CtlKontAll (f k) as vs j
  }

stepFocus :: Exp -> M ()
stepFocus = \case
    ExpFail ->
      setFocus (FocusRet RetValFail)
    ExpAlt x y -> do
      setFocus (FocusRed x)
      ienv <- gets stEnv
      modifyCtlKont $ \j ->
        let k = ctlRedKont j
            a = Alt y ienv k
        in ctlAddAlt a j
    ExpInt k ->
      setFocus (FocusRet (RetValPure (ValInt k)))
    ExpApp a b -> do
      setFocus (FocusRed a)
      modifyRedKont (RedKontAppFirst b)
    ExpOp o ->
      setFocus (FocusRet (RetValPure (ValOp o Empty)))
    ExpTup es ->
      case es of
        Empty -> setFocus (FocusRet (RetValPure (ValTup Empty)))
        e :<| es' -> do
          setFocus (FocusRed e)
          modifyRedKont (RedKontTup Empty es')
    _ -> todo "more focus cases"

stepRet :: RetVal -> M ()
stepRet rv = do
  ctlKont <- gets stCtlKont
  case rv of
    RetValFail -> setFocus (FocusCtl rv)
    RetValPure v -> do
      let redKont = ctlRedKont ctlKont
      case redKont of
        RedKontTop -> setFocus (FocusCtl rv)
        RedKontAlt {} -> todo "ret alt"
        RedKontAppFirst e k ->
          case matchFun v of
            Just f -> do
              setFocus (FocusRed e)
              setRedKont (RedKontAppSecond f k)
            _ -> throwError ErrAppNonFun
        RedKontAppSecond f k -> do
          case f of
            FunOp ar hd tl ->
              if ar == 1
                then applyOp hd (tl :|> v)
                else setFocus (FocusRet (RetValPure (ValOp hd (tl :|> v))))
            FunLam ctx b e -> applyLam ctx b e v
          setRedKont k
        RedKontTup vs es k ->
          case es of
            Empty -> do
              setFocus (FocusRet (RetValPure (ValTup (vs :|> v))))
              setRedKont k
            e :<| es' -> do
              setFocus (FocusRed e)
              setRedKont (RedKontTup (vs :|> v) es' k)

applyOp :: Op -> Seq Val -> M ()
applyOp OpAdd (ValInt a :<| ValInt b :<| Empty) =
  setFocus (FocusRet (RetValPure (ValInt (a + b))))
applyOp OpGt (ValInt a :<| ValInt b :<| Empty) =
  setFocus $ FocusRet $
    if a > b
      then RetValPure (ValInt (a + b))
      else RetValFail
applyOp _ _ = throwError ErrAppOp

applyLam :: Ctx -> TmVar -> Exp -> Val -> M ()
applyLam _ _ _ _ = todo "apply lam"

stepCtl :: RetVal -> M ()
stepCtl rv = do
  ctlKont <- gets stCtlKont
  case ctlKont of
    CtlKontTop -> setFocus (FocusHalt rv)
    CtlKontOne _ as j ->
      case rv of
        RetValPure _ -> setCtlKont j
        RetValFail ->
          case as of
            Empty -> setCtlKont j
            Alt e ienv k :<| as' -> do
              setFocus (FocusRed e)
              setEnv ienv
              setCtlKont (CtlKontOne k as' j)
    CtlKontAll _k _as _vs _j -> todo "ctl all"

data StepRes =
    StepResCont
  | StepResHalt !RetVal
  deriving stock (Eq, Ord, Show)

step :: M StepRes
step = do
  focus <- gets stFocus
  case focus of
    FocusRed e -> StepResCont <$ stepFocus e
    FocusRet rv -> StepResCont <$ stepRet rv
    FocusCtl rv -> StepResCont <$ stepCtl rv
    FocusHalt rv -> pure (StepResHalt rv)

multiStep :: M RetVal
multiStep = do
  res <- step
  case res of
    StepResCont -> multiStep
    StepResHalt rv -> pure rv

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

mergeSeq :: (Int -> Trail -> Trail) -> Seq Val -> Seq Val -> N (Seq Val)
mergeSeq f = go 0 where
  go _ Empty _ = pure Empty
  go _ _ Empty = pure Empty
  go !i (a :<| as) (b :<| bs) = do
    c <- local (f i) (mergeVal a b)
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
        then ValTup <$> mergeSeq TrailTup xvs yvs
        else throwMergeErr (MergeErrTupLen xlen ylen)
    (ValLam {}, ValLam {}) -> throwMergeErr MergeErrLam
    (ValInt xk, ValInt yk) ->
      if xk == yk
        then pure x
        else throwMergeErr (MergeErrInt xk yk)
    (ValOp {}, ValOp {}) -> throwMergeErr MergeErrOp
    _ -> throwMergeErr MergeErrMismatch

mergeUniq :: TmUniq -> TmUniq -> M TmUniq
mergeUniq a b = do
  res <- embedN (UM.stateMerge id mergeVal a b) (TrailStart a b)
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

expTup :: [Exp] -> Exp
expTup = ExpTup . Seq.fromList
