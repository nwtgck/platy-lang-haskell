{-# LANGUAGE DuplicateRecordFields #-}

-- TODO: Don't expose all function
module Platy.Datatypes where

-- | Literal
data Lit =
  IntLit Int   |
  CharLit Char |
  BoolLit Bool |
  UnitLit
  deriving (Show)

-- | Identifier
data Ident = Ident String
  deriving (Show, Eq, Ord)

-- | Type
data Ty =
  IntTy  |
  CharTy |
  BoolTy |
  UnitTy
  deriving (Show)

-- | Bind (definition)
data Bind = Bind {ident :: Ident, ty :: Ty, bodyExpr :: Expr}
  deriving (Show)

-- | Expression
data Expr =
  LitExpr Lit |
  IdentExpr Ident |
  IfExpr {condExpr :: Expr, thenExpr :: Expr, elseExpr :: Expr} |
  ApplyExpr {calleeIdent :: Ident, argExprs :: [Expr]} |
  LetExpr {binds :: [Bind], inExpr :: Expr}
  deriving (Show)

-- | Parameter
data Param = Param {ident :: Ident, ty :: Ty}
  deriving (Show)

-- Global definition
data Gdef =
  LetGdef  {bind :: Bind} |
  FuncGdef {ident :: Ident, params :: [Param], retTy :: Ty, bodyExpr :: Expr}
  deriving (Show)