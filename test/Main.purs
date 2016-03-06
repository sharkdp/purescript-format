module Test.Main where

import Prelude

import Data.Monoid (mempty)

import Text.Format.Number (width, signed, fillZeros, formatInt)

import Test.Unit (test, runTest)
import Test.Unit.Assert (assert, equal)

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

  test "formatInt mempty == show" do
    equal   "123" $ formatInt mempty   123
    equal  "-123" $ formatInt mempty (-123)

  test "formatInt (width)" do
    equal   "  4" $ formatInt (width 3)    4
    equal   " 34" $ formatInt (width 3)   34
    equal   "234" $ formatInt (width 3)  234
    equal  "1234" $ formatInt (width 3) 1234

  test "formatInt (width, negative)" do
    equal  "  -4" $ formatInt (width 4) (   -4)
    equal  " -34" $ formatInt (width 4) (  -34)
    equal  "-234" $ formatInt (width 4) ( -234)
    equal "-1234" $ formatInt (width 4) (-1234)

  test "formatInt (width, signed)" do
    equal   " +4" $ formatInt (signed <> width 3)    4
    equal   "+34" $ formatInt (signed <> width 3)   34
    equal  "+234" $ formatInt (signed <> width 3)  234

  test "formatInt (width, fillZeros)" do
    equal  "-004" $ formatInt (fillZeros <> width 4) (   -4)
    equal  "-034" $ formatInt (fillZeros <> width 4) (  -34)
    equal  "-234" $ formatInt (fillZeros <> width 4) ( -234)
    equal "-1234" $ formatInt (fillZeros <> width 4) (-1234)

  test "formatInt (width, signed, fillZeros)" do
    equal  "+004" $ formatInt (signed <> fillZeros <> width 4)    4
    equal  "+034" $ formatInt (signed <> fillZeros <> width 4)   34
    equal  "+234" $ formatInt (signed <> fillZeros <> width 4)  234
    equal "+1234" $ formatInt (signed <> fillZeros <> width 4) 1234

  test "formatInt (width: special cases)" do
    equal "123" $ formatInt (width     0) 123
    equal "123" $ formatInt (width (-10)) 123

  test "formatInt (signed)" do
    equal "+123" $ formatInt signed   123
    equal "-123" $ formatInt signed (-123)
