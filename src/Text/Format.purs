-- | A module to format strings and numbers in a way similar to `printf` in
-- | C-style languages.

module Text.Format
  ( Properties()
  , width
  , zeroFill
  , signed
  , precision
  , decimalMark
  , class Format
  , format
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Unfoldable (replicate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.String (length, fromCharArray, dropWhile, singleton, replace, Pattern(..), Replacement(..))
import Math (round, pow, abs)

-- | Pad a string on the left up to a given maximum length. The padding
-- | character can be specified.
padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = fromCharArray (replicate (len - length str) c)

type PropertiesRecord =
  { width :: Maybe Int
  , padChar :: Maybe Char
  , signed :: Maybe Boolean
  , precision :: Maybe Int
  , decimalMark :: Maybe Char
  }

default :: PropertiesRecord
default =
  { width: Nothing
  , padChar: Nothing
  , signed: Nothing
  , precision: Nothing
  , decimalMark: Nothing
  }

data Properties = Properties PropertiesRecord

instance eqProperties :: Eq Properties where
  eq (Properties rec1) (Properties rec2) =
    rec1.width     == rec2.width &&
    rec1.padChar   == rec2.padChar &&
    rec1.signed    == rec2.signed &&
    rec1.precision == rec2.precision &&
    rec1.decimalMark == rec2.decimalMark

instance semigroupProperties :: Semigroup Properties where
  append (Properties rec1) (Properties rec2) = Properties rec
    where
      -- These are combined such that options to the right take precedence:
      -- width 3 <> width 4 == width 4
      rec = { width:       rec2.width       <|> rec1.width
            , padChar:     rec2.padChar     <|> rec1.padChar
            , signed:      rec2.signed      <|> rec1.signed
            , precision:   rec2.precision   <|> rec1.precision
            , decimalMark: rec2.decimalMark <|> rec1.decimalMark
            }

instance monoidProperties :: Monoid Properties where
  mempty = Properties default

-- | The minium width of the output.
width :: Int -> Properties
width n = Properties (default { width = Just n })

-- | Fill the free space with zeros instead of spaces.
zeroFill :: Properties
zeroFill = Properties (default { padChar = Just '0' })

-- | Explicitely show a '+' sign for positive numbers. Gets ignored for
-- | non-numeric types.
signed :: Properties
signed = Properties (default { signed = Just true })

-- | Number of decimal places. Gets ignored for non-numeric types.
precision :: Int -> Properties
precision digits = Properties (default { precision = Just digits })

-- | Delimiter character. Gets ignored for non-numeric types.
decimalMark :: Char -> Properties
decimalMark char = Properties (default { decimalMark = Just char })

-- | A class for types that can be formatted using the specified properties.
class Format a where
  format :: Properties -> a -> String

instance formatString :: Format String where
  format (Properties rec) str =
    case rec.width of
      Just len -> padLeft padChar len str
      Nothing -> str
   where
     padChar = fromMaybe ' ' rec.padChar

instance formatInt :: Format Int where
  format prop@(Properties rec) num | fromMaybe 0 rec.precision > 0 =
    format prop (Int.toNumber num)

  format (Properties rec) num =
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
     nonNegative = num >= 0
     numAbs = if nonNegative then num else (-num)
     numSgn = if nonNegative
                then (if isSigned then "+" else "")
                else "-"

instance formatNumber :: Format Number where
  -- Format as an integer if the precision is set to 0
  format prop@(Properties rec) num | rec.precision == Just 0 =
    format prop (Int.round num)

  format (Properties rec) num' =
    case rec.width of
      Just len ->
        if padChar == ' '
          then
            padLeft padChar len (numSgn <> numAbsStr)
          else
            numSgn <> padLeft padChar (len - length numSgn) numAbsStr
      Nothing -> numSgn <> numAbsStr

   where
     num = case rec.precision of
             Nothing -> num'
             Just digits ->
               let f = 10.0 `pow` Int.toNumber digits
               in  round (f * num') / f
     isSigned = fromMaybe false rec.signed
     padChar = fromMaybe ' ' rec.padChar
     nonNegative = num >= 0.0
     numAbsStr'' = show (abs num)
     numAbsStr' = case rec.decimalMark of
                    Nothing -> numAbsStr''
                    Just d -> replace (Pattern ".") (Replacement (singleton d)) numAbsStr''
     numAbsStr = case rec.precision of
                   Nothing -> numAbsStr'
                   Just p -> numAbsStr' <> paddedZeros p
     usedDelimiter = fromMaybe '.' rec.decimalMark
     paddedZeros p = let d = length (dropWhile (_ /= usedDelimiter) numAbsStr') - 1
                     in fromCharArray (replicate (p - d) '0')
     numSgn = if nonNegative
                then (if isSigned then "+" else "")
                else "-"
