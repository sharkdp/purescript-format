module Test.Main where

import Prelude (Unit, discard, mempty, negate, ($), (*), (<>), (==))
import Data.Number (pi)
import Effect (Effect)
import Text.Format (width, signed, zeroFill, precision, decimalMark, format)
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert, equal)


main :: Effect Unit
main = runTest do
  test "Semigroup instance" do
    assert "right option should take precendence" $
      width 2 <> width 4 == width 4

    assert "should be associative" $
      width 2 <> (width 3 <> width 4) == (width 2 <> width 3) <> width 4

  test "Monoid instance" do
    assert "should be a left unit" $
      mempty <> width 4 == width 4

    assert "should be a right unit" $
      width 4 <> mempty == width 4

  test "format mempty == show" do
    equal   "123" $ format mempty   123
    equal  "-123" $ format mempty (-123)
    equal   "foo" $ format mempty "foo"

  test "String: format (width)" do
    equal "  foo" $ format (width 5)   "foo"
    equal "foooo" $ format (width 5) "foooo"

  test "Int: format (width)" do
    equal   "  4" $ format (width 3)    4
    equal   " 34" $ format (width 3)   34
    equal   "234" $ format (width 3)  234
    equal  "1234" $ format (width 3) 1234

  test "Int: format (width, negative)" do
    equal  "  -4" $ format (width 4) (   -4)
    equal  " -34" $ format (width 4) (  -34)
    equal  "-234" $ format (width 4) ( -234)
    equal "-1234" $ format (width 4) (-1234)

  test "Int: format (width, signed)" do
    equal   " +4" $ format (signed <> width 3)    4
    equal   "+34" $ format (signed <> width 3)   34
    equal  "+234" $ format (signed <> width 3)  234

  test "Int: format (width, zeroFill)" do
    equal  "-004" $ format (zeroFill <> width 4) (   -4)
    equal  "-034" $ format (zeroFill <> width 4) (  -34)
    equal  "-234" $ format (zeroFill <> width 4) ( -234)
    equal "-1234" $ format (zeroFill <> width 4) (-1234)

  test "Int: format (width, signed, zeroFill)" do
    equal  "+004" $ format (signed <> zeroFill <> width 4)    4
    equal  "+034" $ format (signed <> zeroFill <> width 4)   34
    equal  "+234" $ format (signed <> zeroFill <> width 4)  234
    equal "+1234" $ format (signed <> zeroFill <> width 4) 1234

  test "Int: format (width: special cases)" do
    equal   "123" $ format (width     0) 123
    equal   "123" $ format (width (-10)) 123

  test "Int: format (signed)" do
    equal  "+123" $ format signed   123
    equal  "-123" $ format signed (-123)

  test "Int: format (precision, decimalMark)" do
    equal  "123,00" $ format (precision 2 <> decimalMark ',') 123

  test "Number: format" do
    equal  "3.14" $ format mempty   3.14
    equal "-3.14" $ format mempty (-3.14)

  test "Number: format (width)" do
    equal " 3.14" $ format (width 5) 3.14

  test "Number: format (zeroFill)" do
    equal "03.14" $ format (zeroFill <> width 5) 3.14
    equal "-3.14" $ format (zeroFill <> width 5) (-3.14)

  test "Number: format (signed)" do
    equal " +3.14" $ format (signed <> width 6) 3.14
    equal "+03.14" $ format (signed <> zeroFill <> width 6) 3.14

  test "Number: format (precision)" do
    equal "3"       $ format (precision 0) pi
    equal "3.1"     $ format (precision 1) pi
    equal "3.14"    $ format (precision 2) pi
    equal "3.14159" $ format (precision 5) pi
    equal "314.159" $ format (precision 3) (pi * 100.0)

    equal "1"       $ format (precision 0) 1.0
    equal "1.0"     $ format (precision 1) 1.0
    equal "1.0000"  $ format (precision 4) 1.0

    -- Format Int as Number:
    equal "3.0"     $ format (precision 1) 3
    equal "3.000"   $ format (precision 3) 3

  test "Number: format (precision, decimalMark)" do
    equal "3"       $ format (precision 0 <> decimalMark ',') pi
    equal "3,1"     $ format (precision 1 <> decimalMark ',') pi

  test "Number: format (width, precision)" do
    equal "  3.14" $ format (width 6 <> precision 2) pi
    equal "     3" $ format (width 6 <> precision 0) pi

  test "Number: format (width, precision, signed)" do
    equal " +3.14" $ format (width 6 <> precision 2 <> signed) pi

  test "Number: format (width, precision, signed, zeroFill)" do
    equal "+03.14" $ format (width 6 <> precision 2 <> signed <> zeroFill) pi
    equal "+3.142" $ format (width 6 <> precision 3 <> signed <> zeroFill) pi

  test "Number: format (width, precision, signed, zeroFill, decimalMark)" do
    equal "+03,14" $ format (width 6 <> precision 2 <> signed <> zeroFill <> decimalMark ',') pi
    equal "+3,142" $ format (width 6 <> precision 3 <> signed <> zeroFill <> decimalMark ',') pi

  test "Handling of zero" do
    equal "0"      $ format mempty 0
    equal "0"      $ format (precision 0) 0.0
    equal "0.00"   $ format (precision 2) 0.0
    equal "0,00"   $ format (precision 2 <> decimalMark ',') 0.0
    equal "+0"     $ format signed 0
    equal "+0.0"   $ format signed 0.0
    equal "+0.00"  $ format (signed <> precision 2) 0
