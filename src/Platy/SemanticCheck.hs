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

-- TODO: impl other patterns

gdefToTypedGdef :: Gdef () -> Gdef Ty
gdefToTypedGdef = undefined

programToTypedProgram :: Program () -> Program Ty
programToTypedProgram = undefined -- TODO: impl