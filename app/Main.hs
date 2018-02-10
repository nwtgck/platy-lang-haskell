{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where


import qualified Data.String.Here as Here
import Control.Monad (mapM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as Map
import Data.Map (Map)

import qualified LLVM.Pretty

import Debug.Trace

import Platy.Datatypes
import Platy.Codegen
import Platy.Utils


main :: IO ()
main = do
  let expr1 = LitExpr (IntLit 1515)
  let Right (operand1, exprCodeEnv1) = exprToOperandEither Map.empty expr1
  print operand1
  print exprCodeEnv1

  let expr2 = IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 81818) (LitExpr $ IntLit 23232)
  let Right (operand2, exprCodeEnv2) = exprToOperandEither Map.empty expr2
  print operand2
  print exprCodeEnv2

  putStrLn("====================================")
  let gdef1 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 81818) (LitExpr $ IntLit 23232)}}
  let gdef2 = LetGdef {bind=Bind {ident=Ident "myint2", ty=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 7117) (LitExpr $ IntLit 9889)}}
  let Right mod1 = gdefsToModule [gdef1, gdef2]
--  TIO.putStrLn (LLVM.Pretty.ppllvm mod1)
  putStrLn("----------------------------------")

  toLLVM mod1

  let gdef3 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=LitExpr $ IntLit 2929}}
  let mod2Either = gdefsToModule [gdef3]
  let Right mod2 = mod2Either
--  print mod2
  putStrLn("----------------------------------")
--  TIO.putStrLn (LLVM.Pretty.ppllvm mod2)
--  putStrLn("----------------------------------")
--  TIO.putStrLn (LLVM.Pretty.ppll mod2)
  toLLVM mod2
  putStrLn("----------------------------------")


  let gdef4 :: Gdef
      gdef4 = FuncGdef {ident=Ident "myfunc", params=[], retTy=IntTy, bodyExpr=LitExpr $ IntLit 32323}
  let Right mod3 = gdefsToModule [gdef4]
  toLLVM mod3
  putStrLn("----------------------------------")


  let gdef5 :: Gdef
      gdef5 = FuncGdef {ident=Ident "myfunc", params=[], retTy=IntTy, bodyExpr=IfExpr (LitExpr $ BoolLit True) (LitExpr $ IntLit 5656) (LitExpr $ IntLit 767)}
  let Right mod4 = gdefsToModule [gdef5]
  toLLVM mod4
  putStrLn("----------------------------------")


  let gdef6 = LetGdef {bind=Bind {ident=Ident "myint", ty=IntTy, bodyExpr=LitExpr $ IntLit 8877}}
  let gdef7 = LetGdef {bind=Bind {ident=Ident "myint2", ty=IntTy, bodyExpr=IdentExpr $ Ident "myint"}}
  let Right mod5 = gdefsToModule [gdef6, gdef7]
  toLLVM mod5
  putStrLn("----------------------------------")

  let gdef8 = LetGdef {bind=Bind {ident=Ident "value1", ty=IntTy, bodyExpr=LetExpr {binds = [Bind{ident=Ident "a", ty=IntTy, bodyExpr=LitExpr $ IntLit 8989}, Bind{ident=Ident "b", ty=IntTy, bodyExpr=LitExpr $ IntLit 3344}], inExpr=IdentExpr (Ident "a")}}}
  let Right mod6 = gdefsToModule [gdef8]
  toLLVM mod6
  putStrLn("----------------------------------")


  let gdef9 = LetGdef {bind=Bind {ident=Ident "value1", ty=IntTy, bodyExpr=LetExpr {binds = [Bind{ident=Ident "a", ty=IntTy, bodyExpr=LitExpr $ IntLit 8989}, Bind{ident=Ident "b", ty=IntTy, bodyExpr=IdentExpr (Ident "a")}], inExpr=IdentExpr (Ident "b")}}}
  let Right mod7 = gdefsToModule [gdef9]
  toLLVM mod7
  putStrLn("----------------------------------")
  TIO.putStrLn (LLVM.Pretty.ppll mod7)
  putStrLn("----------------------------------")

  let gdef11 = FuncGdef {ident=Ident "main", params=[], retTy=UnitTy, bodyExpr=ApplyExpr{calleeIdent=Ident "print-int", argExprs=[LitExpr $ IntLit 171717]}}
  let Right mod8 = gdefsToModule [gdef11]
  toLLVM mod8
  putStrLn("----------------------------------")




