module Text.Format.Number
  ( NumberFormat()
  , signed
  , width
  , fillZeros
  , formatInt
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.String (length)

import Text.Format (padLeft)

data Width
  = Width Int

type NumberFormatRec =
  { signed :: Maybe Boolean
  , padChar :: Maybe Char
  , width :: Maybe Int
  }

default :: NumberFormatRec
default =
  { signed: Nothing
  , padChar: Nothing
  , width: Nothing
  }

data NumberFormat = NumberFormat NumberFormatRec

instance eqNumberFormat :: Eq NumberFormat where
  eq (NumberFormat rec1) (NumberFormat rec2) =
    rec1.signed == rec2.signed &&
    rec1.padChar == rec2.padChar &&
    rec1.width == rec2.width

instance semigroupNumberFormat :: Semigroup NumberFormat where
  append (NumberFormat rec1) (NumberFormat rec2) = NumberFormat rec
    where
      -- These are combined such that options to the right take precedence:
      -- width 3 <> width 4 == width 4
      rec = { signed:  rec2.signed  <|> rec1.signed
            , padChar: rec2.padChar <|> rec1.padChar
            , width:   rec2.width   <|> rec1.width
            }

instance monoidNumberFormat :: Monoid NumberFormat where
  mempty = NumberFormat default

-- | Explicitely show a '+' sign for positive numbers.
signed :: NumberFormat
signed = NumberFormat (default { signed = Just true })

-- | The minium width of the output.
width :: Int -> NumberFormat
width n = NumberFormat (default { width = Just n })

-- | Fill the free space with zeros instead of spaces.
fillZeros :: NumberFormat
fillZeros = NumberFormat (default { padChar = Just '0' })

formatInt :: NumberFormat -> Int -> String
formatInt (NumberFormat rec) num =
  case rec.width of
    Just len ->
      if padChar == ' '
        then
          padLeft padChar len (numSgn <> show numAbs)
        else
          numSgn <> padLeft padChar (len - length numSgn) (show numAbs)
    Nothing -> numSgn <> show numAbs

 where
   isSigned = fromMaybe false rec.signed
   padChar = fromMaybe ' ' rec.padChar
   isPositive = num > 0
   numAbs = if isPositive then num else (-num)
   numSgn = if isPositive
              then (if isSigned then "+" else "")
              else "-"
