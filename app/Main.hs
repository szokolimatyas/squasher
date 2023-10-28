module Main (main) where

import Squasher.Common
import Squasher.Squasher
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Trans.Except(runExcept)

main :: IO ()
main = do
    args <- getArgs
    bytes <- BS.readFile (head args)
    print "Started..."
    case runExcept (runner bytes) of
        Left err -> error err
        Right resNew -> do
--            writeFile "out.txt" ("Aliases:\n" ++ show (aliasEnv res) ++ "\nFunctions:\n" ++ show (tyEnv res))
            writeFile "outnew.txt" ("Aliases:\n" ++ show (aliasEnv resNew) ++ "\nFunctions:\n" ++ show (tyEnv resNew))
            return ()
