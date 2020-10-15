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
  success "" (Program Nothing [] [])
  success (" \n\t  module moduleName .\n\n\t\t\n\t" ++
           " \n\t type \n typeName1  (  \t( (a -> \t    B -> \tC)\t) \n).\n\t\n" ++
           "\n\t\n  \t\n  type      typeName2 \n\n \n \n \t \n \t  XYZ." ++
           "  a \n:- \t\n \t a \n[\n \tC   \n\t\n | \n\tD \n\t ] \n \t,\t \n\tc \t; \nd \n .\t\n "
          )
    (Program (Just "moduleName")
      [TypeDef "typeName1" (Arrow (TAtom (Atom "a" [])) (Arrow (Var "B") (Var "C"))),
        TypeDef "typeName2" (Var "XYZ")]
    [Relation
      (Atom "a" [])
      (Just (Disj (Conj 
              (RAtom (Atom "a" [Left $ Atom "cons" [Right "C", Right "D"]])) (RAtom (Atom "c" [])))
        (RAtom (Atom "d" []))))])

      -- (Program (Just "moduleName")
      --          [TypeDef "typeName1" (Arrow (TAtom $ Atom "a" []) (Arrow (Var "B") (Var "C"))),
      --               TypeDef "typeName2" $ Var "XYZ"]
      --          [Relation (Atom "a" []) (Just $ RAtom $ Atom "b" [])])