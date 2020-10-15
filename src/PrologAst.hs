module PrologAst where

import Data.List

data Token = TIdent String
           | TVar String
           | Comma
           | Semi
           | Lbr
           | Rbr
           | Dot
           | Cork
           deriving (Eq, Show)

data PrologProgram = Program {
        pModule :: Maybe String
      , types   :: [TypeDef]
      , rels    :: [Relation]
      }
      deriving Eq

data TypeDef = TypeDef String Type
             deriving Eq

data Type = Var String
          | TAtom Atom
          | Arrow Type Type
          deriving Eq

data Atom = Atom { atomHead :: String, atomArgs :: [Either Atom String] }
          deriving Eq

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving Eq

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving Eq

cons x y = Atom "cons" [x, y]
nil = Atom "nil" [] 

addIdent :: String -> String
addIdent s = intercalate "\n" $ map ("  " ++) (lines s)

instance Show PrologProgram where
  show (Program pModule types rels) = "Program\n" ++ addIdent (
    "Module\n" ++ addIdent (maybe "Nothing" id pModule) ++
    "\ntypes\n" ++ addIdent (concatMap ((++"\n") . show) types) ++
    "\nrels\n" ++ addIdent (concatMap ((++"\n") . show) rels)
    )

instance Show TypeDef where
  show (TypeDef s t) = "TypeDef\n" ++ addIdent (show s ++ "\n" ++ show t) 

instance Show Type where
  show (Var s) = "Var\n" ++ addIdent (show s)
  show (TAtom a) = "TAtom\n" ++ addIdent (show a) 
  show (Arrow x y) = "Arrow\n" ++ addIdent (show x ++ "\n" ++ show y)

instance Show Atom where
  show (Atom head args) = "Atom\n" ++ addIdent (show head ++ "\n" ++ concatMap ((++"\n") . show) args)

instance Show Relation where
  show (Relation head Nothing) = "Relation\n" ++ addIdent (show head)
  show (Relation head (Just body)) = "Relation\n" ++ addIdent (show head ++ "\n" ++ show body)

instance Show RelationBody where
   show (RAtom a) = "RAtom\n" ++ addIdent (show a)
   show (Conj a b) = "Conj\n" ++ addIdent (show a ++ "\n" ++ show b)
   show (Disj a b) = "Disj\n" ++ addIdent (show a ++ "\n" ++ show b)