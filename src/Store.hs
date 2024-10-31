module Store where

import Data.Map (Map)
import Data.Word (Word64)

type Address = Word64

type Store x = Map Address x
