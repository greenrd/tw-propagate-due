{-# LANGUAGE NoImplicitPrelude #-}
-- | TODO: Contribute these back to non-empty package
module Data.NonEmpty.Set.Utils where

import BasicPrelude hiding (null)
import qualified Data.NonEmpty.Set as NESet
import Data.Set (Set, minView, null)

nonEmptySet :: Ord a => Set a -> Maybe (NESet.T a)
nonEmptySet = fmap (uncurry NESet.insert) . minView

soleMember s = do
           (h, t) <- return $ NESet.minView s
           guard $ null t
           return h
