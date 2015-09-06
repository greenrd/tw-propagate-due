{-# LANGUAGE NoImplicitPrelude #-}
-- | TODO: Contribute these back to non-empty package
module Data.NonEmpty.Set.Utils where

import BasicPrelude hiding (null)
import qualified Data.NonEmpty.Set as NESet
import Data.Set (minView, null)

nonEmptySet :: Ord a => Set a -> Maybe (NESet.T a)
nonEmptySet = fmap (uncurry NESet.insert) . minView

soleMember :: MonadPlus m => NESet.T a -> m a
soleMember s = do
           (h, t) <- return $ NESet.minView s
           guard $ null t
           return h
