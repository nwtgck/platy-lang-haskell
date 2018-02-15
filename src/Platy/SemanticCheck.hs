{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

-- Don't expose everthing
module Platy.SemanticCheck where

import Control.Monad.State
import qualified Data.Char
import qualified Data.String.Here as Here
import Control.Monad (mapM_)
import Control.Monad as Monad
import Data.String      (IsString(..))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Foldable as Foldable
import qualified Control.Monad.Trans as Monad.Trans
import Control.Applicative ((<|>))
import qualified Data.Either.Utils as Either.Utils

import Platy.Datatypes
import qualified Platy.Utils as Utils

-- | Error code
-- TODO: Rename
data ErrorCode2 =
    NoSuchIdentEC
  | TypeMismatchEC
  | ArgsNumMismatchEC
  | UnexpectedEC
  deriving (Eq, Show)

-- | Semantic error
-- TODO: Rename
data SemanticError2 =
 SemanticError2
 { errorCode    :: ErrorCode2
 , errorMessage :: String
 }
 deriving (Eq, Show)

data IdentInfo =
 GVarIdentInfo {ty :: Ty} |
 LVarIdentInfo {ty :: Ty} |
 FuncIdentInfo {retTy :: Ty, paramTys :: [Ty]}
 deriving (Show)

type VarTable = Map Ident IdentInfo

data SemanticCheckEnv =
 SemanticCheckEnv
 { globalVarTable  :: VarTable
 , localVarTables  :: [VarTable]
 }
  deriving (Show)

newtype SemanticCheck a = SemanticCheck {runSemanticCheck :: StateT SemanticCheckEnv (Either SemanticError2) a}
  deriving (Functor, Applicative, Monad, MonadState SemanticCheckEnv)

-- | Literal => Ty
litToTy :: Lit -> Ty
litToTy (IntLit _)  = IntTy
litToTy (CharLit _) = CharTy
litToTy (BoolLit _) = BoolTy
litToTy (UnitLit)   = UnitTy

-- | Expr => LLVM Type
-- TODO: Remove
exprToTy :: VarTable -> [VarTable] -> (Expr ()) -> Either SemanticError2 Ty
exprToTy gVarTable lVarTables (LitExpr {lit})       = return $ litToTy lit
exprToTy gVarTable lVarTables (IdentExpr {ident=ident@(Ident name)}) = do
  let identInfoMaybe = Utils.lookupMaps ident lVarTables <|> Map.lookup ident gVarTable
      notFoundError = SemanticError2{errorCode=NoSuchIdentEC, errorMessage=[Here.i| Identifier '${name}' is not found|]}
  -- Get identifier information
  identInfo <- Either.Utils.maybeToEither notFoundError identInfoMaybe
  case identInfo of
     GVarIdentInfo{ty} -> return ty
     LVarIdentInfo{ty} -> return ty
     FuncIdentInfo{}   -> fail [Here.i| Identifier '${name}' should be variable not function|]
exprToTy gVarTable lVarTables (IfExpr {thenExpr}) = exprToTy gVarTable lVarTables thenExpr
exprToTy gVarTable lVarTables (ApplyExpr{calleeIdent=calleeIdent@(Ident name)}) = do
  let identInfoMaybe = Map.lookup calleeIdent gVarTable
      notFoundError = SemanticError2{errorCode=NoSuchIdentEC, errorMessage=[Here.i| Identifier '${name}' is not found|]}
  -- Get identifier information
  identInfo <- Either.Utils.maybeToEither notFoundError identInfoMaybe
  case identInfo of
    GVarIdentInfo{}      -> fail [Here.i| Identifier '${name}' should be function, but variable|]
    LVarIdentInfo{}      -> fail [Here.i| Unexpected error: '${name}' should be global function|]
    FuncIdentInfo{retTy} -> return retTy
exprToTy gVarTable lVarTables (LetExpr{inExpr})   = exprToTy gVarTable lVarTables inExpr


-- | Push & Pop a local variable map
withLVarTable :: Map Ident IdentInfo -> SemanticCheck a -> SemanticCheck a
withLVarTable lVarMap f = do
  -- Push `lVarMap` to  `localVarTables`
  modify (\env@SemanticCheckEnv{localVarTables} -> env{localVarTables=lVarMap:localVarTables})
  ret <- f
  -- Pop lVarMap` from  `localVarTables`
  modify (\env@SemanticCheckEnv{localVarTables=_:rest} -> env{localVarTables=rest})
  return ret


-- | Expr () => Expr Ty
exprToTypedExpr :: Expr () -> SemanticCheck (Expr Ty)
exprToTypedExpr LitExpr {lit} = return LitExpr{anno=litToTy lit, lit=lit}
exprToTypedExpr IdentExpr{ident=ident@(Ident name)} = do
  -- Get local variable tables
  lVarTables <- gets localVarTables
  -- Get global variable table
  gVarTable  <- gets globalVarTable
  -- Get identifier information
  identInfo <- let identInfoMaybe = Utils.lookupMaps ident lVarTables <|> Map.lookup ident gVarTable
                   notFoundError  = SemanticError2{errorCode=NoSuchIdentEC, errorMessage=[Here.i| Identifier '${name}' is not found|]}
               in SemanticCheck $ Monad.Trans.lift $ Either.Utils.maybeToEither notFoundError identInfoMaybe
  case identInfo of
     GVarIdentInfo{ty} -> return IdentExpr {anno=ty, ident}
     LVarIdentInfo{ty} -> return IdentExpr {anno=ty, ident}
     FuncIdentInfo{}   -> fail [Here.i| Identifier '${name}' should be variable not function|]
exprToTypedExpr IfExpr {condExpr, thenExpr, elseExpr} = do
  -- Condition
  typedCondExpr <- exprToTypedExpr condExpr
  -- Then
  typedThenExpr <- exprToTypedExpr thenExpr
  -- Else
  typedElseExpr <- exprToTypedExpr elseExpr

  let condTy :: Ty
      condTy = anno typedCondExpr
      thenTy :: Ty
      thenTy = anno typedThenExpr
      elseTy :: Ty
      elseTy = anno typedElseExpr

  if condTy == BoolTy -- Condition should be bool
    then do
      if thenTy == elseTy -- types of then and else are the same
        then do
          return IfExpr {anno=thenTy, condExpr=typedCondExpr, thenExpr=typedThenExpr, elseExpr=typedElseExpr}
        else
          SemanticCheck $ Monad.Trans.lift $ Left SemanticError2{errorCode=TypeMismatchEC, errorMessage=[Here.i| Types of then and else should be the same, but then: ${thenTy}, else: ${elseTy} found|]}
    else
      SemanticCheck $ Monad.Trans.lift $ Left SemanticError2{errorCode=TypeMismatchEC, errorMessage=[Here.i| Condtion should be Bool type, but '${condTy}' found|]}
exprToTypedExpr ApplyExpr{calleeIdent=calleeIdent@(Ident name), argExprs} = do
  -- Type argExpr
  typedArgExprs <- mapM exprToTypedExpr argExprs
  -- Get global variable table
  gVarTable <- gets globalVarTable
  -- Get identifier information
  identInfo <- let identInfoMaybe = Map.lookup calleeIdent gVarTable
                   notFoundError  = SemanticError2{errorCode=NoSuchIdentEC, errorMessage=[Here.i| Identifier '${name}' is not found|]}
               in SemanticCheck $ Monad.Trans.lift $ Either.Utils.maybeToEither notFoundError identInfoMaybe
  case identInfo of
    GVarIdentInfo{}      -> fail [Here.i| Identifier '${name}' should be function, but variable|]
    LVarIdentInfo{}      -> fail [Here.i| Unexpected error: '${name}' should be global function|]
    FuncIdentInfo{retTy, paramTys} -> do
      if length paramTys == length typedArgExprs
        then do
          let actualArgTys = fmap anno typedArgExprs
          if paramTys == actualArgTys
          then return ApplyExpr{anno=retTy, calleeIdent, argExprs=typedArgExprs}
          else
            SemanticCheck $ Monad.Trans.lift $ Left SemanticError2{errorCode=TypeMismatchEC, errorMessage=[Here.i| The argument should be '${paramTys}', but '${actualArgTys}'|]}
        else
          SemanticCheck $ Monad.Trans.lift $ Left SemanticError2{errorCode=ArgsNumMismatchEC, errorMessage=[Here.i| The number of arguments should be ${length paramTys}, but ${length typedArgExprs}|]}
exprToTypedExpr LetExpr{binds, inExpr} = do
  -- TODO: Rename better
  let f :: (Map Ident IdentInfo, [Bind Ty]) -> Bind () -> SemanticCheck (Map Ident IdentInfo, [Bind Ty])
      f (lVarMap, typedBinds) (Bind {ident=ident@(Ident name), ty, bodyExpr}) = do
        withLVarTable lVarMap $ do -- NOTE: push & pop (lVarMap)
          -- Type bodyExpr
          typedBodyExpr <- exprToTypedExpr bodyExpr
          let actualBodyTy :: Ty
              actualBodyTy = anno typedBodyExpr
          if ty == actualBodyTy
            then do
              let newlVarMap = Map.insert ident (LVarIdentInfo{ty=ty}) lVarMap
              let typedBind  = Bind {ident, ty, bodyExpr=typedBodyExpr}
              return (newlVarMap, typedBinds ++ [typedBind])
            else
              SemanticCheck $ Monad.Trans.lift $ Left SemanticError2{errorCode=TypeMismatchEC, errorMessage=[Here.i| Type of the expresson should be '${ty}' but found '${actualBodyTy}'|]}

  -- Define all binds
  (localVariableMap, typedBinds) <- Foldable.foldlM f (Map.empty :: Map Ident IdentInfo, [] :: [Bind Ty]) binds
  -- Get typedInExpr
  typedInExpr <- withLVarTable localVariableMap $ do -- NOTE: push & pop (localVariableMap)
    -- Type inExpr
    exprToTypedExpr inExpr
  return LetExpr{anno=anno typedInExpr, binds=typedBinds, inExpr=typedInExpr}

-- | Gdef () => Gdef Ty
gdefToTypedGdef :: VarTable -> Gdef () -> Either SemanticError2 (Gdef Ty)
gdefToTypedGdef globalVarTable gdef =
  case gdef of
    LetGdef{bind=Bind{ident, ty, bodyExpr}} -> do
      let initEnv = SemanticCheckEnv {globalVarTable, localVarTables=[]}
      typedBodyExpr <- evalStateT (runSemanticCheck $ exprToTypedExpr bodyExpr) initEnv
      let actualBodyTy :: Ty
          actualBodyTy  = anno typedBodyExpr
      if ty == actualBodyTy
        then
          return LetGdef{bind=Bind{ident, ty, bodyExpr=typedBodyExpr}}
        else
          Left SemanticError2{errorCode=TypeMismatchEC, errorMessage=[Here.i| Type of the expresson should be '${ty}' but found '${actualBodyTy}'|]}
    FuncGdef {ident, params, retTy, bodyExpr} -> do
      -- Variable of parameters
      let paramVarTable = Map.fromList [(ident, LVarIdentInfo{ty=ty}) | Param{ident, ty} <- params]
      let initEnv = SemanticCheckEnv {globalVarTable, localVarTables=[paramVarTable]}
      typedBodyExpr <- evalStateT (runSemanticCheck $ exprToTypedExpr bodyExpr) initEnv
      let actualBodyTy :: Ty
          actualBodyTy  = anno typedBodyExpr
      if retTy == actualBodyTy
       then
         return FuncGdef {ident, params, retTy, bodyExpr=typedBodyExpr}
       else
         Left SemanticError2{errorCode=TypeMismatchEC, errorMessage=[Here.i| Return-type of the function body should be '${retTy}' but found '${actualBodyTy}'|]}

-- | Program () => Program Ty
programToTypedProgram :: Program () -> Either SemanticError2 (Program Ty)
programToTypedProgram Program{gdefs} = do
  let f LetGdef {bind=Bind {ident, ty}} = (ident, GVarIdentInfo {ty=ty})
      f FuncGdef {ident, retTy, params} = (ident, FuncIdentInfo {retTy=retTy, paramTys=[ty | Param {ty} <- params]})
      -- TODO: <Find duplicated indentifier>
      globalVarMap = Map.fromList (fmap f gdefs)
  --Type gdefs
  typedGdefs <- mapM (gdefToTypedGdef globalVarMap) gdefs
  return Program{gdefs=typedGdefs}