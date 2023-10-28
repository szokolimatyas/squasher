{-# LANGUAGE RecordWildCards   #-}
module Squasher.Output where

import Squasher.Squasher
import Squasher.Common
import qualified Erlang.Type as ET
import Squasher.Types
import Data.Text(Text)
import qualified Data.Text as Text

out :: SquashConfig -> Text
out = undefined

-- so it can be pretty printed
-- we need to also find good names and aliases here
toErl :: SquashConfig -> ([(Text, ET.TypedAttrVal)], [ET.TypeSpec])
toErl SquashConfig{..} = undefined