{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Declaration where

import Data.Binary
import qualified SourceSyntax.Expression as Expr
import qualified SourceSyntax.Type as T
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Declaration' port def
    = Definition def
    | Datatype String [String] [(String,[T.Type])] [String]
    | TypeAlias String [String] T.Type [String]
    | Port port
    | Fixity Assoc Int String
      deriving (Show)

data Assoc = L | N | R
    deriving (Eq)

data ParsePort
    = PPAnnotation String T.Type
    | PPDef String Expr.ParseExpr
      deriving (Show)

data Port
    = Out String Expr.Expr T.Type
    | In String T.Type
      deriving (Show)

portName :: Port -> String
portName port =
    case port of
      Out name _ _ -> name
      In name _ -> name

type ParseDeclaration = Declaration' ParsePort Expr.ParseDef
type Declaration = Declaration' Port Expr.Def

instance Show Assoc where
    show assoc =
        case assoc of
          L -> "left"
          N -> "non"
          R -> "right"

instance Binary Assoc where
    get = do n <- getWord8
             return $ case n of
                0 -> L
                1 -> N
                2 -> R
                _ -> error "Error: invalid Associativity in serialized string"

    put assoc = putWord8 $ case assoc of { L -> 0 ; N -> 1 ; R -> 2 }

instance (Pretty port, Pretty def) => Pretty (Declaration' port def) where
  pretty decl =
    case decl of
      Definition def -> pretty def

      Datatype tipe tvars ctors derivations ->
          P.hang data' 4 (P.sep ctors') $+$ P.nest 4 (prettyDeriving derivations)
          where
            data' = P.text "data" <+> P.text tipe <+> P.hsep (map P.text tvars)
            ctors' = zipWith join ("=" : repeat "|") ctors

            join c ctor = P.text c <+> prettyCtor ctor
            prettyCtor (name, tipes) =
                P.hang (P.text name) 2 (P.sep (map T.prettyParens tipes))

      TypeAlias name tvars tipe derivations ->
          alias $+$ P.nest 4 (prettyDeriving derivations)
          where
            name' = P.text name <+> P.hsep (map P.text tvars)
            alias = P.hang (P.text "type" <+> name' <+> P.equals) 4 (pretty tipe)

      Port port -> pretty port

      Fixity assoc prec op -> P.text "infix" <> assoc' <+> P.int prec <+> P.text op
          where
            assoc' = case assoc of
                       L -> P.text "l"
                       N -> P.empty
                       R -> P.text "r"

prettyDeriving :: [String] -> P.Doc
prettyDeriving deriveables =
    case deriveables of
      [] -> P.empty
      ds -> P.text "deriving" <+> P.hsep (P.punctuate P.comma $ map P.text ds)

instance Pretty ParsePort where
  pretty port =
    case port of
      PPAnnotation name tipe -> prettyPort name ":"  tipe
      PPDef        name expr -> prettyPort name "=" expr

instance Pretty Port where
  pretty port =
    case port of
      In name tipe -> prettyPort name ":" tipe
      Out name expr tipe -> P.vcat [ prettyPort name ":" tipe
                                   , prettyPort name "=" expr ]
          

prettyPort :: (Pretty a) => String -> String -> a -> Doc
prettyPort name op e = P.text "port" <+> P.text name <+> P.text op <+> pretty e
