module Main (main) where

import Squasher.Local(runner, functions, aliases)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Trans.Except(runExcept)

main :: IO ()
main = do
    args <- getArgs
    bytes <- BS.readFile (head args)
    case runExcept (runner bytes) of
        Left err -> error err
        Right res -> do
            putStrLn ("Aliases:\n" ++ show (aliases res))
            putStrLn ("Functions:\n" ++ show (functions res))
            return ()
