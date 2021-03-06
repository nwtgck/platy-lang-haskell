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


    it "apply" $ do
      let expr1   = ApplyExpr
                    { anno = ()
                    , calleeIdent = Ident "myfunc"
                    , argExprs =
                      [ LitExpr
                        { anno = ()
                        , lit = BoolLit False
                        }
                      , LitExpr
                        { anno = ()
                        , lit = CharLit 's'
                        }
                      ]
                    }
      let initEnv = SemanticCheckEnv
                    { globalVarTable =
                      Map.fromList
                        [ ( Ident "myfunc"
                          , FuncIdentInfo
                            { retTy = IntTy
                            , paramTys = [BoolTy, CharTy]
                            })
                        ]
                    , localVarTables = []
                    }
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right ApplyExpr
                          { anno = IntTy
                          , calleeIdent = Ident "myfunc"
                          , argExprs =
                            [ LitExpr
                              { anno = BoolTy
                              , lit = BoolLit False
                              }
                            , LitExpr
                              { anno = CharTy
                              , lit = CharLit 's'
                              }
                            ]
                          }
      actual `shouldBe` expect

    it "let-expression" $ do
      let expr1   = LetExpr
                     { anno = ()
                     , binds =
                       [ Bind
                         { ident = Ident "a"
                         , ty = IntTy
                         , bodyExpr = LitExpr {anno=(), lit=IntLit 889922}
                         }
                       , Bind
                         { ident = Ident "b"
                         , ty = CharTy
                         , bodyExpr = LitExpr {anno=(), lit=CharLit 'j'}
                         }
                       ]
                     , inExpr = IdentExpr{anno=(), ident=Ident "a"}
                     }
      let initEnv = SemanticCheckEnv {globalVarTable=Map.empty, localVarTables=[]}
      let actual  = Monad.State.evalStateT (runSemanticCheck (exprToTypedExpr expr1)) initEnv
      let expect  = Right LetExpr
                           { anno = IntTy
                           , binds =
                             [ Bind
                               { ident = Ident "a"
                               , ty = IntTy
                               , bodyExpr = LitExpr {anno=IntTy, lit=IntLit 889922}
                               }
                             , Bind
                               { ident = Ident "b"
                               , ty = CharTy
                               , bodyExpr = LitExpr {anno=CharTy, lit=CharLit 'j'}
                               }
                             ]
                           , inExpr = IdentExpr{anno=IntTy, ident=Ident "a"}
                           }
      actual `shouldBe` expect

  describe "[positive] programToTypedProgram" $ do
    it "global-let" $ do
      let prog1   = Program
                    { gdefs =
                      [ LetGdef
                        { bind =
                          Bind
                          { ident = Ident "a"
                          , ty = IntTy
                          , bodyExpr = LitExpr {anno=(), lit=IntLit 337733}
                          }
                        }
                      , LetGdef
                        { bind =
                          Bind
                          { ident = Ident "b"
                          , ty = IntTy
                          , bodyExpr = LitExpr {anno=(), lit=IntLit 909}
                          }
                        }
                      ]
                    }
      let actual  = programToTypedProgram prog1
      let expect  = Right Program
                          { gdefs =
                            [ LetGdef
                              { bind =
                                Bind
                                { ident = Ident "a"
                                , ty = IntTy
                                , bodyExpr = LitExpr {anno=IntTy, lit=IntLit 337733}
                                }
                              }
                            , LetGdef
                              { bind =
                                Bind
                                { ident = Ident "b"
                                , ty = IntTy
                                , bodyExpr = LitExpr {anno=IntTy, lit=IntLit 909}
                                }
                              }
                            ]
                          }
      actual `shouldBe` expect

    it "func" $ do
        let prog1   = Program
                      { gdefs =
                        [ FuncGdef
                          { ident = Ident "myfunc"
                          , params = [Param {ident=Ident "p", ty=IntTy}, Param {ident=Ident "q", ty=CharTy}]
                          , retTy = IntTy
                          , bodyExpr = IdentExpr{anno=(), ident=Ident "p"}
                          }
                        ]
                      }
        let actual  = programToTypedProgram prog1
        let expect  = Right Program
                            { gdefs =
                              [ FuncGdef
                                { ident = Ident "myfunc"
                                , params = [Param {ident=Ident "p", ty=IntTy}, Param {ident=Ident "q", ty=CharTy}]
                                , retTy = IntTy
                                , bodyExpr = IdentExpr{anno=IntTy, ident=Ident "p"}
                                }
                              ]
                            }
        actual `shouldBe` expect