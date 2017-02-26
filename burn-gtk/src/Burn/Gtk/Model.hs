module Burn.Gtk.Model where

import Burn.Types
import Control.Lens
import Data.Default

-- | Model of fields stored in view
data Model = Model
  { _mTags :: Tags
  } deriving (Eq, Ord, Show)

makeLenses ''Model

instance Default Model where
  def = Model $ Tags []
