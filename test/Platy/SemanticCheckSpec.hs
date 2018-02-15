{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}


module Platy.SemanticCheckSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.String.Here as Here
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Control.Monad.State as Monad.State

import qualified LLVM.Pretty

import Debug.Trace

import Platy.Datatypes
--import Platy.Codegen
import Platy.SemanticCheck
import Platy.Utils
import qualified Platy.TestUtils as  TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "[positive] exprToTypedExpr" $ do
    it "int literal" $ do
      let expr1   = LitExpr {anno=(), lit=IntLit 3232}
      let initEnv = SemanticCheckEnv {globalVarTable=Map.empty, localVarTables=[]}
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right LitExpr {anno=IntTy, lit=IntLit 3232}
      actual `shouldBe` expect

    it "char literal" $ do
      let expr1   = LitExpr {anno=(), lit=CharLit 'm'}
      let initEnv = SemanticCheckEnv {globalVarTable=Map.empty, localVarTables=[]}
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right LitExpr {anno=CharTy, lit=CharLit 'm'}
      actual `shouldBe` expect

    it "local identifier" $ do
      let expr1   = IdentExpr {anno=(), ident=Ident "a"}
      let initEnv = SemanticCheckEnv {globalVarTable=Map.empty, localVarTables=[Map.fromList [(Ident "a", LVarIdentInfo{ty=IntTy})] ]}
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right IdentExpr {anno=IntTy, ident=Ident "a"}
      actual `shouldBe` expect

    it "global identifier" $ do
      let expr1   = IdentExpr {anno=(), ident=Ident "a"}
      let initEnv = SemanticCheckEnv {globalVarTable=Map.fromList [(Ident "a", GVarIdentInfo{ty=IntTy})], localVarTables=[]}
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right IdentExpr {anno=IntTy, ident=Ident "a"}
      actual `shouldBe` expect

    it "if expression" $ do
      let expr1   = IfExpr
                    { anno = ()
                    , condExpr =
                      LitExpr
                      { anno = ()
                      , lit = BoolLit True
                      }
                    , thenExpr =
                      LitExpr
                      { anno = ()
                      , lit = IntLit 18181
                      }
                    , elseExpr =
                      LitExpr
                      { anno = ()
                      , lit = IntLit 2332
                      }
                    }
      let initEnv = SemanticCheckEnv {globalVarTable=Map.empty, localVarTables=[]}
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right IfExpr
                          { anno = IntTy
                          , condExpr =
                            LitExpr
                            { anno = BoolTy
                            , lit = BoolLit True
                            }
                          , thenExpr =
                            LitExpr
                            { anno = IntTy
                            , lit = IntLit 18181
                            }
                          , elseExpr =
                            LitExpr
                            { anno = IntTy
                            , lit = IntLit 2332
                            }
                          }
      actual `shouldBe` expect