{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- TODO: Don't expose all function
module Platy.Parser where

import           Text.Parsec      (Parsec, (<|>))
import qualified Text.Parsec      as Parsec
import qualified Text.Parsec.Char as ParsecChar

import Platy.Datatypes

-- | Prefix of keywords (reserved world)
keywordPrefixChar :: Char
keywordPrefixChar = '@'

-- | One-line comment char
onelineCommentChar = ';'

-- | Int type name
intTyNameStr = "Int"

-- | Bool type name
boolTyNameStr = "Bool"

-- | Char type name
charTyNameStr = "Char"

-- | Unit type name
unitTyNameStr = "Unit"

-- | Unit literal name
unitLitStr    = "Unit"

-- | True literal name
trueLitStr    = "True"

-- | False literal name
falseLitStr   = "False"


-- | Consume a white or comment
skipLangSpaceP :: Parsec String u ()
skipLangSpaceP = (Parsec.space *> return ()) <|> do
  Parsec.char onelineCommentChar
  Parsec.manyTill Parsec.anyChar (Parsec.endOfLine *> return () <|> Parsec.eof)
  return ()

-- | Between '(' and ')'
betweenParens :: Parsec String u a -> Parsec String u a
betweenParens = Parsec.between (ParsecChar.char '(' >> Parsec.skipMany skipLangSpaceP) (Parsec.skipMany skipLangSpaceP >> ParsecChar.char ')')

-- | List of p Parser
listP :: Parsec String u a -> Parsec String u [a]
listP p = do
  ParsecChar.char '['
  Parsec.skipMany skipLangSpaceP
  as <- Parsec.sepBy p (Parsec.skipMany1 skipLangSpaceP)
  Parsec.skipMany skipLangSpaceP
  ParsecChar.char ']'
  return as




-- | Parser of identifier
identP :: Parsec String u Ident
identP = do
  start <- ParsecChar.letter <|> ParsecChar.char '_'
  rest  <- Parsec.many (ParsecChar.alphaNum <|> ParsecChar.char '_')
  return $ Ident (start:rest)


-- | Parser of type
tyP :: Parsec String u Ty
tyP = intTyP <|> boolTyP <|> charTyP <|> unitTyP
  where
    intTyP  = ParsecChar.string intTyNameStr  >> return IntTy
    boolTyP = ParsecChar.string boolTyNameStr >> return BoolTy
    charTyP = ParsecChar.string charTyNameStr >> return CharTy
    unitTyP = ParsecChar.string unitTyNameStr >> return UnitTy

-- | Parser of literal
litP :: Parsec String u Lit
litP = intLitP <|> boolLitP <|> charLitP <|> unitLitP
  where
    intLitP = do
      intStr <- Parsec.many1 (ParsecChar.digit)
      return (IntLit (read intStr))

    boolLitP = trueLitP <|> falseLitP
      where
        trueLitP  = ParsecChar.string trueLitStr  >> return (BoolLit True)
        falseLitP = ParsecChar.string falseLitStr >> return (BoolLit False)

    charLitP = do
      ParsecChar.char '\''
      ch <- ParsecChar.anyChar
      ParsecChar.char '\''
      return (CharLit ch)

    unitLitP = ParsecChar.string unitLitStr >> return UnitLit

-- | Parser of expression
exprP :: Parsec String u Expr
exprP = litExprP <|> identExprP <|> betweenParens (applyExprP <|> (ParsecChar.char keywordPrefixChar *> (ifExprP <|> letExprP)))
  where
    -- | Parser of literal expression
    litExprP = do
      lit <- litP
      return $ LitExpr lit

    -- | Parser of identifier expression
    identExprP = do
      ident <- identP
      return $ IdentExpr ident

    -- | Parser of if expression
    ifExprP = do
      -- @if
      ParsecChar.string ("if") -- TODO: Hard code
      Parsec.skipMany1 skipLangSpaceP
      -- condition expression
      condExpr <- exprP
      Parsec.skipMany1 skipLangSpaceP
      -- then expression
      thenExpr <- exprP
      Parsec.skipMany1 skipLangSpaceP
      -- else expression
      elseExpr <- exprP
      return IfExpr {condExpr, thenExpr, elseExpr}

    -- | Parser of let-expression
    letExprP = do
      -- let
      ParsecChar.string "let" -- TODO: Hard code
      Parsec.skipMany1 skipLangSpaceP
      -- binds
      binds <- listP (betweenParens (ParsecChar.string "::" *> Parsec.skipMany1 skipLangSpaceP *> bindP)) -- TODO: Hard code
      Parsec.skipMany1 skipLangSpaceP
       -- expression
      inExpr <- exprP
      return $ LetExpr {binds, inExpr}

    -- | Parser of apply expression
    applyExprP = do
      calleeIdent <- identP
      Parsec.skipMany1 skipLangSpaceP
      argExprs <- listP exprP
      return ApplyExpr {calleeIdent, argExprs}

-- Parser of param
paramP :: Parsec String u Param
paramP = betweenParens $ do
  -- ::
  ParsecChar.string "::" -- TODO: Hard code
  Parsec.skipMany1 skipLangSpaceP
  -- identifier
  ident <- identP
  Parsec.skipMany1 skipLangSpaceP
  -- type
  ty   <- tyP
  return Param{ident, ty}

-- | Parser of bind
bindP :: Parsec String u Bind
bindP = do
  -- identifier
  ident <- identP
  Parsec.skipMany1 skipLangSpaceP
  -- type
  ty   <- tyP
  Parsec.skipMany1 skipLangSpaceP
  -- expression
  bodyExpr <- exprP
  return $ Bind{ident, ty, bodyExpr}

-- | Parser of Global Definition
gdefP :: Parsec String u Gdef
gdefP = betweenParens (ParsecChar.char keywordPrefixChar *> (letGdefP <|> funcGdefP))
  where
    letGdefP  = do
      -- @global-let
      ParsecChar.string "global-let" -- TODO: Hard code
      Parsec.skipMany1 skipLangSpaceP
      -- Bind
      bind <- bindP
      return $ LetGdef {bind}

    funcGdefP = do
      -- @func
      ParsecChar.string "func" -- TODO: Hard code
      Parsec.skipMany1 skipLangSpaceP
      -- identifier
      ident <- identP
      Parsec.skipMany1 skipLangSpaceP
      -- parameters
      params <- listP paramP
      Parsec.skipMany1 skipLangSpaceP
      -- type
      retTy  <- tyP
      Parsec.skipMany1 skipLangSpaceP
      -- expression
      bodyExpr <- exprP
      return $ FuncGdef {ident, params, retTy, bodyExpr}


-- | Parser of program
programP :: Parsec String u Program
programP = do
  Parsec.skipMany skipLangSpaceP
  gdefs <- Parsec.many (gdefP <* Parsec.many skipLangSpaceP)
  return $ Program {gdefs}