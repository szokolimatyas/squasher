module Main (main) where

import Squasher.Common
import Squasher.Squasher
import Squasher.Output
import Squasher.Naming
import           Foreign.Erlang.Term
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Trans.Except(runExcept)
import           Data.Binary                             (decodeOrFail, encode)
-- import qualified Data.Text as Text

main :: IO ()
main = do
    args <- getArgs
    bytes <- BS.readFile (head args)
    case decodeOrFail bytes of
        Left (_, _, str) -> error $ "Could not parse bytestring, error: " ++ str
        Right (_, _, MkExternalTerm (List terms Nil)) -> do
            print "Started..."
            case runExcept (runner terms) of
                Left err -> error err
                Right resNew -> do
                    writeFile "outnew.txt" ("Aliases:\n" ++ show (aliasEnv resNew) ++ "\nFunctions:\n" ++ show (tyEnv resNew))
                    BS.writeFile "out.bin" $ encode $ MkExternalTerm $ out (nameAll resNew) resNew
                    return ()
        Right (_, _, MkExternalTerm terms) -> error $ "Terms are in a wrong format: " ++ show terms
