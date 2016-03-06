module Text.Format
  ( padLeft
  ) where

import Prelude

import Data.String (length, fromCharArray)
import Data.Array (replicate)

-- | Pad a string on the left up to a given maximum length. The padding
-- | character can be specified.
padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = fromCharArray (replicate (len - length str) c)
