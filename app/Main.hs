{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad.Trans.Except (runExcept)
import           Data.Binary                (decodeOrFail, encode)
import qualified Data.ByteString.Lazy       as BS
import           Foreign.Erlang.Term
import           Options.Applicative
import           Data.IntMap                (IntMap)
import           Data.Text                  (Text)
import           Squasher.Common
import           Squasher.Naming
import           Squasher.Output
import           Squasher.Squasher
import           System.Process.Typed
import           Control.Monad(when)

main :: IO ()
main = do
    o <- execParser opts
    bytes <- BS.readFile $ inputPath o
    putStrLn "Input read"
    case decodeOrFail bytes of
        Left (_, _, err) -> error $ "Could not parse bytestring, error: " ++ err
        Right (_, _, MkExternalTerm (List terms Nil)) -> do
            putStrLn "Started..."
            case runExcept (runner o terms) of
                Left err -> error err
                Right (old, resOld, resNew) -> do
                    writeFile "unprocessed.txt" ("\nFunctions:\n" ++ show old)
                    writeFile "out.txt" ("Aliases:\n" ++ show (aliasEnv resOld) ++ "\nFunctions:\n" ++ show (tyEnv resOld))
                    when (printUnformatted o) $
                        writeFile "outnew.txt" ("Aliases:\n" ++ show (aliasEnv resNew) ++ "\nFunctions:\n" ++ show (tyEnv resNew))
                    names <- parameters resNew
                    BS.writeFile "out.bin" $ encode $ MkExternalTerm $ out (nameAll names resNew) resNew
                    writePretty $ prettyOutputPath o
                    return ()
        Right (_, _, MkExternalTerm terms) -> error $ "Terms are in a wrong format: " ++ show terms
    where
        opts = info (optionsP <**> helper)
            ( fullDesc
            <> progDesc "Format specs from runtime debugging results"
            <> header "Squasher - dynamic inference for Erlang" )

parameters :: SquashConfig -> IO (IntMap Text)
parameters conf@SquashConfig{options=Options{parametersPath=Just path}} = do
    putStrLn $ "Reading parameter names from: " ++ path
    bytes <- BS.readFile path
    case decodeOrFail bytes of
        Left (_, _, err) -> error $ "Could not parse bytestring, error: " ++ err
        Right (_, _, MkExternalTerm terms) ->
            return $ paramNames conf terms
parameters _ = return emptyParamNames

writePretty :: String -> IO ()
writePretty path = do
    putStrLn $ "Writing formatted results to " ++ path
    -- todo: don't crash
    let p = proc "erl" ["-noinput", "-eval", "{ok, B} = file:read_file(\"out.bin\"), L = binary_to_term(B), S = lists:flatmap(fun(F) -> erl_pp:form(F) ++ \"\n\" end, L), file:write_file(\"" <> path <> "\", S), init:stop()."]
    code <- runProcess p
    case code of
        ExitSuccess -> do
            putStrLn "Done"
            return ()
        _ -> error "Could not pretty print results"

strategyP :: Parser Strategy
strategyP = option auto
    (  long "strategy"
    <> short 's'
    <> metavar "S1|S2|S3"
    <> help "Global squashing strategy"
    <> showDefault
    <> value S2
    )

optionsP :: Parser Options
optionsP = Options
    <$> strategyP
    <*> option auto
        ( long "mixedAtoms"
        <> help "Upcast mixed atom unions"
        <> metavar "BOOL"
        <> showDefault
        <> value True
        )
    <*> option auto
        ( long "mixedTuples"
        <> help "Upcast mixed tuple unions"
        <> metavar "BOOL"
        <> showDefault
        <> value True
        )
    <*> option auto
        ( long "atomSize"
        <> short 'a'
        <> metavar "INT"
        <> help "Upcast atom unions larger than this"
        <> showDefault
        <> value 3
        )
    <*> option auto
        ( long "tupleSize"
        <> short 't'
        <> metavar "INT"
        <> help "Upcast similar tuple unions larger than this"
        <> showDefault
        <> value 0
        )
    <*> option auto
        ( long "recordSize"
        <> short 'r'
        <> metavar "INT"
        <> help "Recordify tagged tuples with as least this many fields"
        <> showDefault
        <> value 3
        )
    <*> switch
        ( long "unformatted"
        <> short 'u'
        <> help "Dump unformatted debug info"
        )
    <*> option (Just <$> str)
        ( long "parameters"
        <> short 'p'
        <> metavar "FILE"
        <> help "File containing parameter information for a module"
        <> showDefault
        <> value Nothing
        )
    <*> option str
        ( long "out"
        <> short 'o'
        <> metavar "FILE"
        <> help "Where to put pretty printed output"
        )
    <*> argument str
        (metavar "FILE")
