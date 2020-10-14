module Test.PrologParserMyTests where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Text.ParserCombinators.Parsec
import Data.Either (isLeft)

import PrologParser
import PrologAst
import Test.PrologParser


unit_atom :: Assertion
unit_atom = do
  let parser = atom
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a [X]" (Atom "a" [l $ cons (r "X") (l nil)])
  fail "[X] a"

unit_list :: Assertion
unit_list = do
  let parser = list
  let success = testParserSuccess parser
  let fail = testParserFailure parser
  fail "[a, b | T]"

unit_prog :: Assertion
unit_prog = do
  let parser = prog
  let success = testParserSuccess parser
  let fail = testParserFailure parser
  undefined