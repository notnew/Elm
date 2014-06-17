{-# OPTIONS_GHC -Wall #-}
module Transform.Substitute (subst) where

import Control.Arrow (second, (***))
import qualified Data.Set as Set

import AST.Annotation
import AST.Expression.General (Expr'(..))
import AST.Expression.Valid (CanonicalExpr', Cmd(..))
import qualified AST.Expression.Valid as Valid
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var

subst :: String -> CanonicalExpr' -> CanonicalExpr' -> CanonicalExpr'
subst old new expr =
    let f (A a e) = A a (subst old new e)

        shadow p = Set.member old (Pattern.boundVars p)

        hasShadow (Valid.Definition p _ _) = shadow p

        substDef (Valid.Definition pattern e tipe) =
            Valid.Definition pattern (f e) tipe
    in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      ExplicitList es -> ExplicitList (map f es)
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      Lambda p e
          | shadow p -> expr
          | otherwise -> Lambda p (f e)
      App e1 e2 -> App (f e1) (f e2)
      MultiIf ps -> MultiIf (map (f *** f) ps)

      Let defs body
          | any hasShadow defs -> expr
          | otherwise -> Let (map substDef defs) (f body)

      With impl command ->
          With (f impl) (substCmd command)
          where
            substCmd cmd =
              case cmd of
                Do e -> Do (f e)

                DoAnd e c ->
                    DoAnd (f e) (substCmd c)

                CmdLet defs c
                    | any hasShadow defs -> cmd
                    | otherwise -> CmdLet (map substDef defs) (substCmd c)

                AndThen p e t c
                    | shadow p -> cmd
                    | otherwise -> AndThen p (f e) t (substCmd c)

      Var (Var.Canonical home x) ->
          case home of
            Var.Module _ -> expr
            Var.BuiltIn -> expr
            Var.Local -> if x == old then new else expr

      Case e cases -> Case (f e) $ map (second f) cases
      Data name es -> Data name (map f es)
      Access e x -> Access (f e) x
      Remove e x -> Remove (f e) x
      Insert e x v -> Insert (f e) x (f v)
      Modify r fs -> Modify (f r) (map (second f) fs)
      Record fs -> Record (map (second f) fs)
      Literal _ -> expr
      Markdown uid md es -> Markdown uid md (map f es)
      GLShader _ _ _ -> expr
      PortIn name st -> PortIn name st
      PortOut name st signal -> PortOut name st (f signal)
