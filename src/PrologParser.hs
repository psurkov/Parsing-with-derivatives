module PrologParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower, isUpper)
import PrologAst

languageDef =
  emptyDef { Token.identStart = lower
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = ["module", "type"]
           , Token.reservedOpNames = [",", ";", "->", ":-"]
           }

lexer = Token.makeTokenParser languageDef

identifier = do
  i <- Token.identifier lexer
  guard $ isLower $ head i
  return i

var :: Parser [Char]
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h:t)

whiteSpace = Token.whiteSpace lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
parens = Token.parens lexer
brackets = Token.brackets lexer
dot = Token.dot lexer
comma = Token.comma lexer
semi = Token.semi lexer

cork :: Parser [Char]
cork = do
  spaces
  string ":-"
  spaces
  return ":-"

arrow :: Parser [Char]
arrow = do
  spaces
  string "->"
  spaces
  return "->"

pipe :: Parser Char
pipe = do
  spaces
  char '|'
  spaces
  return '|'


atom :: Parser Atom
atom = do
  atomHead <- identifier
  spaces
  atomArgs <- many (
                      (do ind <- identifier; spaces; return $ Left $ Atom ind []) <|>
                      (do v <- var; return $ Right v) <|>
                      (Left <$> atomInParens) <|>
                      (Left <$> list)
                   )
  return $ Atom atomHead atomArgs

atomInParens :: Parser Atom
atomInParens = parens (atomInParens <|> atom)


relationBodyItem :: Parser RelationBody
relationBodyItem = (parens relationBody) <|> (RAtom <$> atom)

relationBodyConj :: Parser RelationBody
relationBodyConj = do
  spaces
  l <- relationBodyItem
  maybeConj <- optionMaybe (do comma; relationBodyConj)
  return $ maybe l (Conj l) maybeConj

relationBody :: Parser RelationBody
relationBody = do
  spaces
  l <- relationBodyConj
  maybeDisj <- optionMaybe (do semi; relationBody)
  return $ maybe l (Disj l) maybeDisj

relation :: Parser Relation
relation = do
  spaces
  relHead <- atom
  relBody <- optionMaybe (do cork; relationBody)
  dot
  return $ Relation relHead relBody


parseModule :: Parser String
parseModule = do
  spaces
  reserved "module"
  spaces
  ind <- identifier
  spaces
  dot
  return ind

typeExprItem :: Parser Type
typeExprItem = (Var <$> var) <|> (TAtom <$> atom) <|> (parens typeExpr)

typeExpr :: Parser Type
typeExpr = do
  spaces
  l <- typeExprItem
  maybeArrow <- optionMaybe (do arrow; typeExpr)
  return $ maybe l (Arrow l) maybeArrow

typ :: Parser TypeDef
typ = do
  spaces
  reserved "type"
  ind <- identifier
  expr <- typeExpr
  dot
  return $ TypeDef ind expr

listElem :: Parser (Either Atom String)
listElem = do
  spaces
  (Left <$> list) <|> (Left <$> atom) <|> (Right <$> var)

maybeIsNothing :: Maybe a -> Bool
maybeIsNothing Nothing = True
maybeIsNothing _       = False

list :: Parser Atom
list = brackets (do
  elems <- sepBy listElem (string ",")
  maybeSpecification <- optionMaybe (do pipe; var)
  guard (maybeIsNothing maybeSpecification || length elems == 1)
  tail <- return $ maybe (Left nil) Right maybeSpecification
  return $ case foldr (\elem xs -> Left (cons elem xs)) tail elems of Left x -> x
  )

prog :: Parser PrologProgram
prog = do
  pModule <- optionMaybe parseModule
  types <- many typ
  rels <- many relation
  return $ Program pModule types rels