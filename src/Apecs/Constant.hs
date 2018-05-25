module Apecs.Constant (type Constant) where

import           Control.DeepSeq

import qualified Apecs.Slice     as Slice
import           Apecs.Types

import           Data.IVar       (IVar)
import qualified Data.IVar       as IVar

-- | A write-once global store. Writing to it more than once will throw an error.
newtype Constant a = Constant { constVal :: IVar a }

instance NFData a => Store (Constant a) where
  type Stores (Constant a) = a
  type SafeRW (Constant a) = a
  initStore = Constant <$> IVar.new
  explGet (Constant v) _ = IVar.get v
  explSet (Constant v) _ = IVar.fill v
  explDestroy _ _ = pure ()
  explMembers = Slice.empty
  explGetUnsafe = explGet
  explSetMaybe = explSet

instance NFData a => GlobalStore (Constant a)
