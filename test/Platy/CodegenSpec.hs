{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}


module Platy.CodegenSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.String.Here as Here
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Map (Map)

import qualified LLVM.Pretty

import Debug.Trace

import Platy.Datatypes
import Platy.Codegen
import Platy.Utils
import qualified Platy.TestUtils as  TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Execution Test" $ do
    it "simple-print main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (print-int 171717)
      -- )
      let gdef1 = FuncGdef
                   { ident = Ident "main"
                   , params = []
                   , retTy = UnitTy
                   , bodyExpr =
                     ApplyExpr
                     { calleeIdent = Ident "print-int"
                     , argExprs = [LitExpr $ IntLit 171717]
                     }
                   }

      -- Execute and Get stdout
      stdout <- TestUtils.execGdefs [gdef1]
      -- Compare with expectation
      stdout `shouldBe` "171717\n"

    it "if-true main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (print-int (@if True
      --      8877
      --      5566
      --    ))
      -- )
      let gdef1 = FuncGdef
                    { ident = Ident "main"
                    , params = []
                    , retTy = UnitTy
                    , bodyExpr =
                      ApplyExpr
                      { calleeIdent = Ident "print-int"
                      , argExprs =
                        [ IfExpr
                          { condExpr = LitExpr $ BoolLit True
                          , thenExpr = LitExpr $ IntLit 8877
                          , elseExpr = LitExpr $ IntLit 5566
                          }
                        ]
                      }
                    }

      -- Execute and Get stdout
      stdout <- TestUtils.execGdefs [gdef1]
      -- Compare with expectation
      stdout `shouldBe` "8877\n"

    it "global-identifier main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let gval1 Int 29292)
      -- (@global-let main Unit
      --    (print-int gval1)
      -- )
      let gdef1 = LetGdef
                  { bind =
                    Bind
                    { ident = Ident "gval1"
                    , ty = IntTy
                    , bodyExpr = LitExpr $ IntLit 29292
                    }
                  }
      let gdef2 = FuncGdef
                    { ident = Ident "main"
                    , params = []
                    , retTy = UnitTy
                    , bodyExpr =
                      ApplyExpr
                      { calleeIdent = Ident "print-int"
                      , argExprs =
                        [ IdentExpr $ Ident "gval1"
                        ]
                      }
                    }

      -- Execute and Get stdout
      stdout <- TestUtils.execGdefs [gdef1, gdef2]
      -- Compare with expectation
      stdout `shouldBe` "29292\n"

    it "local-identifier main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let gval1 Int 29292)
      -- (@global-let main Unit
      --    (@let [
      --      (= a Int 9898)
      --      (= b Int 21212)
      --      (= __dummy1__ Unit (print-int a))
      --      (= __dummy2__ Unit (print-int b))
      --    ] Unit)
      -- )
      let gdef1 = FuncGdef
                  { ident = Ident "main"
                  , params = []
                  , retTy = UnitTy
                  , bodyExpr =
                    LetExpr
                    { binds =
                      [ Bind
                        { ident = Ident "a"
                        , ty = IntTy
                        , bodyExpr = LitExpr $ IntLit 9898
                        }
                      , Bind
                        { ident = Ident "b"
                        , ty = IntTy
                        , bodyExpr = LitExpr $ IntLit 21212
                        }
                      , Bind
                        { ident = Ident "__dummy1__"
                        , ty = UnitTy
                        , bodyExpr =
                          ApplyExpr
                          { calleeIdent = Ident "print-int"
                          , argExprs = [IdentExpr $ Ident "a"]
                          }
                        }
                      , Bind
                        { ident = Ident "__dummy2__"
                        , ty = UnitTy
                        , bodyExpr =
                          ApplyExpr
                          { calleeIdent = Ident "print-int"
                          , argExprs = [IdentExpr $ Ident "b"]
                          }
                        }
                      ]
                    , inExpr = LitExpr UnitLit
                    }
                  }
      -- Execute and Get stdout
      stdout <- TestUtils.execGdefs [gdef1]
      -- Compare with expectation
      stdout `shouldBe` "9898\n21212\n"
