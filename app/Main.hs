{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import qualified Data.String.Here as Here
import Control.Monad (mapM_)
import qualified Options.Applicative as OptApplicative
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified Data.ByteString as ByteString
import qualified Text.Parsec      as Parsec
import qualified System.FilePath.Posix as FilePath.Posix
import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import Data.Version (showVersion)
import qualified Data.OpenUnion as OpenUnion
import Data.OpenUnion ((@>))
import Data.Either.Combinators (mapLeft)

import qualified LLVM.AST as AST

import Platy.Datatypes
import Platy.Codegen
import Platy.Utils
import Platy.Parser
import Platy.SemanticCheck

import Paths_platy_lang (version)

data PlatyOptions = PlatyOptions
  { quiet         :: Bool
  , emitLLVM      :: Bool
  , outputPathMay :: Maybe FilePath
  , platyFilePath :: FilePath
  }

-- (from: https://qiita.com/philopon/items/a29717af62831d3c8c07)
platyOptionsP :: OptApplicative.Parser PlatyOptions
platyOptionsP = PlatyOptions
    <$>
    (
      OptApplicative.switch $ mconcat [
        OptApplicative.long "quiet",
        OptApplicative.help "quiet (no output)"
      ]
    )
    <*>
    (
      OptApplicative.switch $ mconcat [
        OptApplicative.long "emit-llvm",
        OptApplicative.help "emit LLVM IR"
      ]
    )
    <*>
    (
      OptApplicative.optional $ OptApplicative.strOption $ mconcat [
        OptApplicative.short 'o',
        OptApplicative.long "output",
        OptApplicative.help "output file path"
      ]
    )
    <*>
    (OptApplicative.strArgument $ mconcat
      [ OptApplicative.help ".platy file"
      , OptApplicative.metavar "PLATY_FILE"
      , OptApplicative.action "file"
      ]
    )

platyOptionsPInfo :: OptApplicative.ParserInfo PlatyOptions
platyOptionsPInfo = OptApplicative.info (OptApplicative.helper <*> versionP <*> platyOptionsP) OptApplicative.fullDesc
  where
    -- (from: https://haskell-lang.org/library/optparse-applicative)
    versionP :: OptApplicative.Parser (a -> a)
    versionP = OptApplicative.infoOption (showVersion version)
                 (mconcat [
                   OptApplicative.short 'v',
                   OptApplicative.long "version",
                   OptApplicative.help "Show version"
                 ])

-- | Compile error
type CompileError = OpenUnion.Union '[Parsec.ParseError, SemanticError, CodegenError]

main :: IO ()
main = do
  -- Parse options
  PlatyOptions{platyFilePath, emitLLVM, quiet, outputPathMay} <- OptApplicative.execParser platyOptionsPInfo
  -- Get code string
  platyCode <- readFile platyFilePath
  -- Flow of compile
  let compileFlowEither :: Either CompileError AST.Module
      compileFlowEither = do
        -- Parse code
        program      <- mapLeft OpenUnion.liftUnion (Parsec.parse programP platyFilePath platyCode)
        -- Semantic analysis
        typedProgram <- mapLeft OpenUnion.liftUnion (programToTypedProgram program)
        -- Generate LLVM module
        llvmModule   <- mapLeft OpenUnion.liftUnion (programToModule typedProgram)
        return llvmModule

  case compileFlowEither of
    Right llvmModule -> do
      if emitLLVM
        then do
          -- Print LLVM IR
          toLLVM llvmModule
        else do
          -- Generate object byte string
          objBString <- toObjByteString llvmModule
          -- Create temp directory
          Temp.withSystemTempDirectory "tempdir" $ \dirpath -> do
            -- Create empty obj file
            objfilePath <- Temp.emptyTempFile dirpath "objfile"
            -- Save obj to a file
            ByteString.writeFile objfilePath objBString
            -- Create empty executable file
            let execfilePath = Maybe.fromMaybe (FilePath.Posix.takeBaseName platyFilePath) outputPathMay
             -- Make executable file
            Process.system [Here.i|gcc ${objfilePath} -o ${execfilePath}|]
            Monad.when (not quiet) $
            -- Print generated message
              putStrLn [Here.i|Executable '${execfilePath}' generated|]
            return ()
    Left compileError -> do
      let errorPrint :: CompileError -> IO ()
          errorPrint
            =  (\(parseError    :: Parsec.ParseError) -> print parseError)
            @> (\(semanticError :: SemanticError)     -> print semanticError)
            @> (\(codegenError  :: CodegenError)      -> print codegenError)
            @> OpenUnion.typesExhausted
      errorPrint compileError
      -- TODO: Print to stdrr
      -- TODO: Exit with error






