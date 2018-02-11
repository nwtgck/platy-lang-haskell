{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Platy.ParserSpec where

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
      let expect = Right $ ApplyExpr {calleeIdent=(Ident "myfunc"), argExprs=[LitExpr (IntLit 78)]}
      Parsec.parse exprP "" "(myfunc [78])" `shouldBe` expect

    it "(@if True 5544 2299)" $ do
      let expect = Right $ IfExpr {condExpr=LitExpr $ BoolLit True, thenExpr=LitExpr $ IntLit 5544, elseExpr=LitExpr $ IntLit 2299}
      Parsec.parse exprP "" "(@if True 5544 2299)" `shouldBe` expect


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