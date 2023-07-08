{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Set
import Squasher.Local (shouldMerge)
import Squasher.Types

main :: IO ()
main = 
    mergeTest

mergeTest :: IO ()
mergeTest = do
    let b = shouldMerge 
                [ETuple [ENamedAtom "leaf", EInt], 
                 (EUnion $ Data.Set.fromList [ETuple [ENamedAtom "leaf", EInt], ETuple [ENamedAtom "leaf1", EInt]])]
    putStrLn $ "false == " ++ show b
    return ()