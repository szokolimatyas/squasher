module Main (main) where

import Squasher.Local(runner, tyEnv, aliasEnv)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Trans.Except(runExcept)

main :: IO ()
main = do
    args <- getArgs
    bytes <- BS.readFile (head args)
    case runExcept (runner bytes) of
        Left err -> error err
        Right (res, resNew) -> do
            writeFile "out.txt" ("Aliases:\n" ++ show (aliasEnv res) ++ "\nFunctions:\n" ++ show (tyEnv res))
            writeFile "outnew.txt" ("Aliases:\n" ++ show (aliasEnv resNew) ++ "\nFunctions:\n" ++ show (tyEnv resNew))
            return ()
