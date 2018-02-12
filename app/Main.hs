{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where


import qualified Data.String.Here as Here
import Control.Monad (mapM_)
import qualified Options.Applicative as OptApplicative
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified Data.ByteString as ByteString
import qualified Text.Parsec      as Parsec
import qualified System.FilePath.Posix as FilePath.Posix

import Platy.Datatypes
import Platy.Codegen
import Platy.Utils
import Platy.Parser

data PlatyOptions = PlatyOptions
  { emitLLVM      :: Bool
  , platyFilePath :: FilePath
  }

-- (from: https://qiita.com/philopon/items/a29717af62831d3c8c07)
platyOptionsP :: OptApplicative.Parser PlatyOptions
platyOptionsP = PlatyOptions
    <$>
    (
      OptApplicative.switch $ mconcat [
        OptApplicative.long "emit-llvm",
        OptApplicative.help "emit LLVM IR"
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
platyOptionsPInfo = OptApplicative.info (OptApplicative.helper <*> platyOptionsP) OptApplicative.fullDesc

main :: IO ()
main = do
  -- Parse options
  PlatyOptions{platyFilePath, emitLLVM} <- OptApplicative.execParser platyOptionsPInfo
  -- Get code string
  platyCode <- readFile platyFilePath
  -- Parse code
  let programEither = Parsec.parse programP platyFilePath platyCode
  case programEither of
    Right program -> do
     -- Generate LLVM module
     let llvmModuleEither = programToModule program
     case llvmModuleEither of
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
              let execfilePath = FilePath.Posix.takeBaseName platyFilePath
               -- Make executable file
              Process.system [Here.i|gcc ${objfilePath} -o ${execfilePath}|]
              -- Print generated message
              putStrLn [Here.i|Executable '${execfilePath}' generated|]
              return ()

       Left err -> putStrLn err

    Left parseErr -> print parseErr






