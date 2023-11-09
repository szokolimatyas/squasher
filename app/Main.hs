{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad.Trans.Except (runExcept)
import           Data.Binary                (decodeOrFail, encode)
import qualified Data.ByteString.Lazy       as BS
import           Foreign.Erlang.Term
import           Squasher.Common
import           Squasher.Naming
import           Squasher.Output
import           Squasher.Squasher
import           System.Environment         (getArgs)
import           System.Process.Typed
-- import qualified Data.Text as Text

main :: IO ()
main = do
    args <- getArgs
    bytes <- BS.readFile (head args)
    case decodeOrFail bytes of
        Left (_, _, str) -> error $ "Could not parse bytestring, error: " ++ str
        Right (_, _, MkExternalTerm (List terms Nil)) -> do
            putStrLn "Started..."
            case runExcept (runner terms) of
                Left err -> error err
                Right resNew -> do
                    writeFile "outnew.txt" ("Aliases:\n" ++ show (aliasEnv resNew) ++ "\nFunctions:\n" ++ show (tyEnv resNew))
                    BS.writeFile "out.bin" $ encode $ MkExternalTerm $ out (nameAll resNew) resNew
                    writePretty
                    return ()
        Right (_, _, MkExternalTerm terms) -> error $ "Terms are in a wrong format: " ++ show terms

writePretty :: IO ()
writePretty = do
    putStrLn "Writing formatted results to file..."
    let p = proc "erl" ["-noshell", "-eval", "{ok, B} = file:read_file(\"out.bin\"), L = binary_to_term(B), S = lists:flatmap(fun(F) -> erl_pp:form(F) ++ \"\n\" end, L), file:write_file(\"pretty.erl\", S), init:stop()."]
    code <- runProcess p
    case code of
        ExitSuccess -> do
            putStrLn "Done"
            return ()
        _ -> error "Could not pretty print results"
