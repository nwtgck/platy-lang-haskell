{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Platy.ParseAndCompileAndRunSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.String.Here as Here
import qualified Data.Either as Either
import qualified Control.Monad as Monad
import qualified System.FilePath.Posix as FilePath.Posix

import Platy.Datatypes
import Platy.Utils
import Platy.Parser
import Platy.SemanticCheck
import Platy.Codegen
import qualified Platy.TestUtils as  TestUtils


import           Text.Parsec      (Parsec, (<|>))
import qualified Text.Parsec      as Parsec
import qualified Text.Parsec.Char as ParsecChar

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "[should pass] Parse & Compile & Run" $ do
    let fileDir         = FilePath.Posix.joinPath ["platy_programs", "should_pass"]
    let programNames    = ["print_int_29", "print_multi_ints", "eq_int_10_9", "print_multi_add_ints", "print_multi_sub_ints", "or_True_False", "fib"] -- NOTE: All you have to do is adding here
    let platyExtension  = "platy"
    let expectExtension = "expect"


    Monad.forM_ programNames $ \programName -> do
      let codeFileName   = [Here.i|${programName}.${platyExtension}|]
          expectFileName = [Here.i|${programName}.${expectExtension}|]
          codeFilePath   =  FilePath.Posix.joinPath [fileDir, codeFileName]
          expectFilePath =  FilePath.Posix.joinPath [fileDir, expectFileName]

      it [Here.i|File - ${codeFileName}|] $ do
        -- Get string from file
        code   <- readFile codeFilePath
        -- Get expectation
        expect <- readFile expectFilePath
        -- Parse code
        let programEither = Parsec.parse programP "" code
        programEither `shouldSatisfy` Either.isRight
        -- Extract program
        let Right program = programEither
        -- Execute and Get stdout
        stdout <- TestUtils.execProgram program
        -- Compare with expectation
        stdout `shouldBe` expect

  describe "[should semantic error] Parse & Compile & Run" $ do
    let fileDir         = FilePath.Posix.joinPath ["platy_programs", "should_be_semantic_error"]
    let programNames    = [ "no_such_ident"
                          , "no_such_ident_callee"
                          , "global_type_mismatch"
                          , "local_type_mismatch"
                          , "if_cond_type_mismatch"
                          , "if_then_type_mismatch"
                          , "func_body_type_mismatch"
                          , "apply_arg_type_mismatch"
                          , "duplicate_global_ident"
                          , "duplicate_func_param"
                          ] -- NOTE: All you have to do is adding here
    let platyExtension  = "platy"
    let expectExtension = "error_code.expect"

    Monad.forM_ programNames $ \programName -> do
      let codeFileName   = [Here.i|${programName}.${platyExtension}|]
          expectFileName = [Here.i|${programName}.${expectExtension}|]
          codeFilePath   =  FilePath.Posix.joinPath [fileDir, codeFileName]
          expectFilePath =  FilePath.Posix.joinPath [fileDir, expectFileName]

      it [Here.i|File - ${codeFileName}|] $ do
        -- Get string from file
        code        <- readFile codeFilePath
        -- Get expectation of error code string
        expectECStr <- readFile expectFilePath
        -- Parse code
        let programEither = Parsec.parse programP "" code
        programEither `shouldSatisfy` Either.isRight
        -- Extract program
        let Right program = programEither
        -- Type program
        let typedProgramEither = programToTypedProgram program
        -- Is right or not
        typedProgramEither `shouldSatisfy` Either.isLeft
        -- Extract error
        let Left SemanticError{errorCode} = typedProgramEither
        -- String expression of error code
        let errorCodeStr = show errorCode
        -- Compare with expectation
        errorCodeStr `shouldBe` expectECStr
