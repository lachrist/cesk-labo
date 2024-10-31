{-# LANGUAGE FlexibleInstances #-}

module Environment where

import Data.Map (Map)
import Expression (Variable)

type Environment v = (Map Variable v)
