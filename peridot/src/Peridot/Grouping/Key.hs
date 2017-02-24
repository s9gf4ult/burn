module Peridot.Grouping.Key where


data Group (root :: * -> *) val where
  GroupAbs :: (Ord val) => root val -> Group root val
  -- ^ Grouping by absolute value. Each group in result will have
  -- records having same value by specified key
