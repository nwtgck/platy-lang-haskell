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
                     { anno=()
                     , calleeIdent = Ident "print-int"
                     , argExprs = [LitExpr {anno=(), lit=IntLit 171717}]
                     }
                   }

      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1]}
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
                      { anno=()
                      , calleeIdent = Ident "print-int"
                      , argExprs =
                        [ IfExpr
                          { anno=()
                          , condExpr = LitExpr {anno=(), lit=BoolLit True}
                          , thenExpr = LitExpr {anno=(), lit=IntLit 8877}
                          , elseExpr = LitExpr {anno=(), lit=IntLit 5566}
                          }
                        ]
                      }
                    }

      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1]}
      -- Compare with expectation
      stdout `shouldBe` "8877\n"

    it "identifier in if main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let a Int 330055)
      -- (@global-let b Int 880099)
      -- (@global-let main Unit
      --    (print-int (@if True a b))
      -- )
      let gdef1 = LetGdef
                  { bind =
                    Bind
                    { ident = Ident "a"
                    , ty = IntTy
                    , bodyExpr = LitExpr {anno=(), lit=IntLit 330055}
                    }
                  }
      let gdef2 = LetGdef
                  { bind =
                    Bind
                    { ident = Ident "b"
                    , ty = IntTy
                    , bodyExpr = LitExpr {anno=(), lit=IntLit 880099}
                    }
                  }
      let gdef3 = FuncGdef
                  { ident = Ident "main"
                  , params = []
                  , retTy = UnitTy
                  , bodyExpr =
                    ApplyExpr
                    { anno=()
                    , calleeIdent = Ident "print-int"
                    , argExprs =
                      [ IfExpr
                        { anno=()
                        , condExpr = LitExpr {anno=(), lit=BoolLit True}
                        , thenExpr = IdentExpr {anno=(), ident=Ident "a"}
                        , elseExpr = IdentExpr {anno=(), ident=Ident "b"}
                        }
                      ]
                    }
                  }
      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1, gdef2, gdef3]}
      -- Compare with expectation
      stdout `shouldBe` "330055\n"


    it "print func in if main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (@if True
      --      (print-int 339911)
      --      (print-int 220022)
      --    )
      -- )
      let gdef1 = FuncGdef
                  { ident = Ident "main"
                  , params = []
                  , retTy = UnitTy
                  , bodyExpr =
                    IfExpr
                    { anno=()
                    , condExpr = LitExpr {anno=(), lit=BoolLit True}
                    , thenExpr =
                      ApplyExpr
                      { anno=()
                      , calleeIdent = Ident "print-int"
                      , argExprs = [LitExpr {anno=(), lit=IntLit 339911}]
                      }
                    , elseExpr =
                      ApplyExpr
                      { anno=()
                      , calleeIdent = Ident "print-int"
                      , argExprs = [LitExpr {anno=(), lit=IntLit 220022}]
                      }
                    }
                  }
      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1]}
      -- Compare with expectation
      stdout `shouldBe` "339911\n"


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
                    , bodyExpr = LitExpr {anno=(), lit=IntLit 29292}
                    }
                  }
      let gdef2 = FuncGdef
                    { ident = Ident "main"
                    , params = []
                    , retTy = UnitTy
                    , bodyExpr =
                      ApplyExpr
                      { anno=()
                      , calleeIdent = Ident "print-int"
                      , argExprs =
                        [ IdentExpr {anno=(), ident=Ident "gval1"}
                        ]
                      }
                    }

      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1, gdef2]}
      -- Compare with expectation
      stdout `shouldBe` "29292\n"

    it "local-identifier main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
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
                    { anno=()
                    , binds =
                      [ Bind
                        { ident = Ident "a"
                        , ty = IntTy
                        , bodyExpr = LitExpr {anno=(), lit=IntLit 9898}
                        }
                      , Bind
                        { ident = Ident "b"
                        , ty = IntTy
                        , bodyExpr = LitExpr {anno=(), lit=IntLit 21212}
                        }
                      , Bind
                        { ident = Ident "__dummy1__"
                        , ty = UnitTy
                        , bodyExpr =
                          ApplyExpr
                          { anno=()
                          , calleeIdent = Ident "print-int"
                          , argExprs = [IdentExpr {anno=(), ident=Ident "a"}]
                          }
                        }
                      , Bind
                        { ident = Ident "__dummy2__"
                        , ty = UnitTy
                        , bodyExpr =
                          ApplyExpr
                          { anno=()
                          , calleeIdent = Ident "print-int"
                          , argExprs = [IdentExpr {anno=(), ident=Ident "b"}]
                          }
                        }
                      ]
                    , inExpr = LitExpr {anno=(), lit=UnitLit}
                    }
                  }
      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1]}
      -- Compare with expectation
      stdout `shouldBe` "9898\n21212\n"

    it "nested-if main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (print-int (@if False
      --      2299
      --      (@if True
      --        6633
      --        9900
      --      )
      --    ))
      -- )
      let gdef1 = FuncGdef
                    { ident = Ident "main"
                    , params = []
                    , retTy = UnitTy
                    , bodyExpr =
                      ApplyExpr
                      { anno=()
                      , calleeIdent = Ident "print-int"
                      , argExprs =
                        [ IfExpr
                          { anno=()
                          , condExpr = LitExpr {anno=(), lit=BoolLit False}
                          , thenExpr = LitExpr {anno=(), lit=IntLit 2299}
                          , elseExpr =
                            IfExpr
                            { anno=()
                            , condExpr = LitExpr {anno=(), lit=BoolLit True}
                            , thenExpr = LitExpr {anno=(), lit=IntLit 6633}
                            , elseExpr = LitExpr {anno=(), lit=IntLit 9900}
                            }
                          }
                        ]
                      }
                    }

      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1]}
      -- Compare with expectation
      stdout `shouldBe` "6633\n"


    it "nested-let main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (@let [
      --      (= a Int 9911)
      --      (= b Int (@let [
      --        (= c Int 669944)
      --        (= d Int 881122)
      --      ] d))
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
                    { anno=()
                    , binds =
                      [ Bind
                        { ident = Ident "a"
                        , ty = IntTy
                        , bodyExpr = LitExpr {anno=(), lit=IntLit 9911}
                        }
                      , Bind
                        { ident = Ident "b"
                        , ty = IntTy
                        , bodyExpr =
                          LetExpr
                          { anno=()
                          , binds =
                            [ Bind
                              { ident = Ident "c"
                              , ty = IntTy
                              , bodyExpr = LitExpr {anno=(), lit=IntLit 669944}
                              }
                            , Bind
                              { ident = Ident "d"
                              , ty = IntTy
                              , bodyExpr = LitExpr {anno=(), lit=IntLit 881122}
                              }
                            ]
                          , inExpr = IdentExpr {anno=(), ident=Ident "d"}
                          }
                        }
                      , Bind
                        { ident = Ident "__dummy1__"
                        , ty = UnitTy
                        , bodyExpr =
                          ApplyExpr
                          { anno=()
                          , calleeIdent = Ident "print-int"
                          , argExprs = [IdentExpr {anno=(), ident=Ident "a"}]
                          }
                        }
                      , Bind
                        { ident = Ident "__dummy2__"
                        , ty = UnitTy
                        , bodyExpr =
                          ApplyExpr
                          { anno=()
                          , calleeIdent = Ident "print-int"
                          , argExprs = [IdentExpr {anno=(), ident=Ident "b"}]
                          }
                        }
                      ]
                    , inExpr = LitExpr {anno=(), lit=UnitLit}
                    }
                  }
      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1]}
      -- Compare with expectation
      stdout `shouldBe` "9911\n881122\n"


    it "func-def main" $ do
      -- [corresponding platy code] NOTE: Syntax maybe wrong
      -- (@global-let main Unit
      --    (print-int 171717)
      -- )
      let gdef1 = FuncGdef
                   { ident = Ident "myfunc"
                   , params = [Param {ident=Ident "a", ty=IntTy}, Param {ident=Ident "b", ty=IntTy}]
                   , retTy = UnitTy
                   , bodyExpr =
                     ApplyExpr
                     { anno=()
                     , calleeIdent = Ident "print-int"
                     , argExprs = [IdentExpr {anno=(), ident=Ident "b"}]
                     }
                   }

      let gdef2 = FuncGdef
                   { ident = Ident "main"
                   , params = []
                   , retTy = UnitTy
                   , bodyExpr =
                     ApplyExpr
                     { anno=()
                     , calleeIdent = Ident "myfunc"
                     , argExprs = [LitExpr {anno=(), lit=IntLit 449999}, LitExpr {anno=(), lit=IntLit 3300}]
                     }
                   }

      -- Execute and Get stdout
      stdout <- TestUtils.execProgram Program{gdefs=[gdef1, gdef2]}
      -- Compare with expectation
      stdout `shouldBe` "3300\n"