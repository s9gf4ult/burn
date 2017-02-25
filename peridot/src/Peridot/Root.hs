module Peridot.Root where

import Peridot.Grouping
import Peridot.Statistics

data Root (payload :: * -> *) val where
  Payload  :: payload val                   -> Root payload val
  Stats    :: Statistics (Root payload) val -> Root payload val
  Grouping :: Group (Root payload) val      -> Root payload val
