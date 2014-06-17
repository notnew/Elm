{-# OPTIONS_GHC -Wall #-}

module AST.Expression.Source where

import AST.PrettyPrint
import Text.PrettyPrint as P
import qualified AST.Expression.General as General
import AST.Type (RawType)
import qualified AST.Variable as Var
import qualified AST.Annotation as Annotation
import qualified AST.Pattern as Pattern


{-| Expressions created by the parser. These use a split representation of type
annotations and definitions, which is how they appear in source code and how
they are parsed.
-}
type Expr = General.Expr Annotation.Region Def Cmds Var.Raw
type Expr' = General.Expr' Annotation.Region Def Cmds Var.Raw

data Def
    = Definition Pattern.RawPattern Expr
    | TypeAnnotation String RawType
    deriving (Show)

data Cmds = Cmds [Cmd]
    deriving (Show)

data Cmd
    = Do Expr
    | CmdLet [CmdDef]
    deriving (Show)

data CmdDef
    = Assign  Pattern.RawPattern Expr
    | AndThen Pattern.RawPattern Expr
    | TypeAnn String RawType
    deriving (Show)

instance Pretty Def where
  pretty def =
   case def of
     TypeAnnotation name tipe -> prettyType name tipe
     Definition pattern expr  -> prettyDef pattern "=" expr

prettyDef :: Pattern.RawPattern -> String -> Expr -> Doc
prettyDef pattern eq expr =
    pretty pattern <+> P.text eq <+> pretty expr

prettyType :: String -> RawType -> Doc
prettyType name tipe =
    variable name <+> P.colon <+> pretty tipe

instance Pretty Cmds where
  pretty (Cmds cmds) =
      P.vcat (map prettyCommand cmds)

prettyCommand :: Cmd -> Doc
prettyCommand command =
  case command of
    Do expr     -> pretty expr
    CmdLet defs -> P.text "let" <+> P.vcat (map cmdDef defs)
  where
    cmdDef def =
        case def of
          Assign  p e -> prettyDef p "=" e
          AndThen p e -> prettyDef p "<-" e
          TypeAnn n t -> prettyType n t
