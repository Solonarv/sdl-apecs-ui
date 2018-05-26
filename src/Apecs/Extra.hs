module Apecs.Extra where

import           Data.Kind

import           Apecs

type family HasAll (w :: *) (cs :: [*]) :: Constraint where
  HasAll w '[] = ()
  HasAll w (c ': cs) = (Has w c, HasAll w cs)

getWorld :: System w w
getWorld = System ask
