module Main (main) where

import Squasher.Local(runner)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    args <- getArgs
    bytes <- BS.readFile (head args)
    case runner bytes of
        Nothing -> error "Could not squash types"
        Just res -> print $ show res
