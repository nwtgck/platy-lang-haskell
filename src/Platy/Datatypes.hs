{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}

-- TODO: Don't expose all function
module Platy.Datatypes where

-- | Literal
data Lit =
  IntLit Int   |
  CharLit Char |
  BoolLit Bool |
  UnitLit
  deriving (Show, Eq)

-- | Identifier
data Ident = Ident String
  deriving (Show, Eq, Ord)

-- | Type
data Ty =
  IntTy  |
  CharTy |
  BoolTy |
  UnitTy
  deriving (Show, Eq)

-- | Bind (definition)
data Bind anno = Bind {ident :: Ident, ty :: Ty, bodyExpr :: Expr anno}
  deriving (Show, Eq)

-- | Expression
data Expr anno =
  LitExpr   {anno :: anno, lit :: Lit } |
  IdentExpr {anno :: anno, ident :: Ident} |
  IfExpr    {anno :: anno, condExpr :: Expr anno, thenExpr :: Expr anno, elseExpr :: Expr anno} |
  ApplyExpr {anno :: anno, calleeIdent :: Ident, argExprs :: [Expr anno]} |
  LetExpr   {anno :: anno, binds :: [Bind anno], inExpr :: Expr anno}
  deriving (Show, Eq)


-- | Parameter
data Param = Param {ident :: Ident, ty :: Ty}
  deriving (Show, Eq)


-- | Global definition
data Gdef anno =
  LetGdef  {bind :: Bind anno} |
  FuncGdef {ident :: Ident, params :: [Param], retTy :: Ty, bodyExpr :: Expr anno}
  deriving (Show, Eq)

-- | Program (maybe rename to Module or Package)
data Program anno = Program {gdefs :: [Gdef anno]}
  deriving (Show, Eq)