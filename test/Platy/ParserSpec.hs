{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Platy.ParserSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.String.Here as Here

import Platy.Datatypes
import Platy.Utils
import Platy.Parser
import qualified Platy.TestUtils as  TestUtils


import           Text.Parsec      (Parsec, (<|>))
import qualified Text.Parsec      as Parsec
import qualified Text.Parsec.Char as ParsecChar

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse Expression Test" $ do
    it "23 - Int literal" $ do
      let expect = Right $ LitExpr $ IntLit 23
      Parsec.parse exprP "" "23" `shouldBe` expect

    it "'k' - Char literal" $ do
      let expect = Right $ LitExpr $ CharLit 'k'
      Parsec.parse exprP "" "'k'" `shouldBe` expect

    it "'myVar' - Identifier" $ do
      let expect = Right $ IdentExpr $ Ident "myVar"
      Parsec.parse exprP "" "myVar" `shouldBe` expect

    it "(myfunc [78]) - Apply" $ do
      let expect = Right $ ApplyExpr
                           { calleeIdent = (Ident "myfunc")
                           , argExprs = [LitExpr (IntLit 78)]
                           }
      Parsec.parse exprP "" "(myfunc [78])" `shouldBe` expect

    it "(@if True 5544 2299)" $ do
      let expect = Right $ IfExpr
                           { condExpr = LitExpr $ BoolLit True
                           , thenExpr = LitExpr $ IntLit 5544
                           , elseExpr = LitExpr $ IntLit 2299
                           }
      Parsec.parse exprP "" "(@if True 5544 2299)" `shouldBe` expect

    it "let-expression" $ do
      let expect = Right $ LetExpr
                           { binds =
                             [ Bind
                               { ident = Ident "a"
                               , ty = IntTy
                               , bodyExpr = LitExpr $ IntLit 2233
                               }
                             , Bind
                               { ident = Ident "b"
                               , ty = CharTy
                               , bodyExpr = LitExpr $ CharLit 'f'
                               }
                             ]
                           , inExpr = IdentExpr $ Ident "a"
                           }
      Parsec.parse exprP "" "(@let [(= a Int 2233) (= b Char 'f')] a)" `shouldBe` expect


  describe "Parse Global Definition" $ do
    it "@global-let" $ do
      let expect = Right $ LetGdef
                           { bind =
                             Bind
                             { ident = Ident "myGlobal"
                             , ty = IntTy
                             , bodyExpr = LitExpr $ IntLit 445544
                             }
                           }
      Parsec.parse gdefP "" "(@global-let myGlobal Int 445544)" `shouldBe` expect

    it "@func" $ do
      let expect = Right $ FuncGdef
                            { ident = Ident "myfunc"
                            , params = [Param {ident=Ident "a", ty=IntTy}, Param {ident=Ident "b", ty=CharTy}]
                            , retTy = IntTy
                            , bodyExpr = IdentExpr $ Ident "a"
                            }
      Parsec.parse gdefP "" "(@func myfunc [(:: a Int) (:: b Char)] Int a)" `shouldBe` expect


  describe "Parse Program" $ do
    it "empty program" $ do
      let expect = Right Program {gdefs=[]}
      let code   = ""
      Parsec.parse programP "" code `shouldBe` expect

    it "multi definitions" $ do
        let expect = Right Program
                            { gdefs =
                              [ LetGdef
                                { bind =
                                  Bind
                                  { ident = Ident "a"
                                  , ty = IntTy
                                  , bodyExpr = LitExpr $ IntLit 8989
                                  }
                                }
                              , LetGdef
                                { bind =
                                  Bind
                                  { ident = Ident "b"
                                  , ty = IntTy
                                  , bodyExpr = LitExpr $ IntLit 12121
                                  }
                                }
                              ]
                            }
        let code = [Here.here|

(@global-let a Int 8989)

(@global-let b Int 12121)

|]
        Parsec.parse programP "" code `shouldBe` expect


    it "multi definitions with comments" $ do
        let expect = Right Program
                            { gdefs =
                              [ LetGdef
                                { bind =
                                  Bind
                                  { ident = Ident "a"
                                  , ty = IntTy
                                  , bodyExpr = LitExpr $ IntLit 8989
                                  }
                                }
                              , LetGdef
                                { bind =
                                  Bind
                                  { ident = Ident "b"
                                  , ty = IntTy
                                  , bodyExpr = LitExpr $ IntLit 12121
                                  }
                                }
                              ]
                            }
        let code = [Here.here|

; Global variable (this is a comment)
(@global-let a Int 8989)


(@global-let b Int 12121) ; This is global variable
|]
        Parsec.parse programP "" code `shouldBe` expect