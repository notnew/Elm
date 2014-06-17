{-# OPTIONS_GHC -Wall #-}
module Transform.Declaration (combineAnnotations, toExpr) where

import Control.Applicative ((<$>))

import qualified AST.Annotation as A
import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as S
import qualified AST.Expression.Valid as V
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as Var

import qualified Transform.Definition as Def


combineAnnotations :: [D.SourceDecl] -> Either String [D.ValidDecl]
combineAnnotations decls =
  let go = combineAnnotations
      msg x = "Formatting Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."
  in
  case decls of
    -- simple cases, pass them through with no changes
    [] -> return []

    D.Datatype name tvars ctors : rest ->
        (:) (D.Datatype name tvars ctors) <$> go rest

    D.TypeAlias name tvars alias : rest ->
        (:) (D.TypeAlias name tvars alias) <$> go rest

    D.Fixity assoc prec op : rest ->
        (:) (D.Fixity assoc prec op) <$> go rest

    -- combine definitions
    D.Definition def : defRest ->
        case def of
          S.Definition pat expr ->
              do expr' <- Def.validateExpr expr
                 let def' = V.Definition pat expr' Nothing
                 (:) (D.Definition def') <$> go defRest

          S.TypeAnnotation name tipe ->
              case defRest of
                D.Definition (S.Definition pat@(P.Var name') expr) : rest
                    | name == name' ->
                        do expr' <- Def.validateExpr expr
                           let def' = V.Definition pat expr' (Just tipe)
                           (:) (D.Definition def') <$> go rest

                _ -> Left (msg name)

    -- combine ports
    D.Port port : portRest ->
        case port of
          D.PPDef name _ -> Left (msg name)
          D.PPAnnotation name tipe ->
              case portRest of
                D.Port (D.PPDef name' expr) : rest | name == name' ->
                    do expr' <- Def.validateExpr expr
                       (:) (D.Port (D.Out name expr' tipe)) <$> go rest

                _ -> (:) (D.Port (D.In name tipe)) <$> go portRest


toExpr :: String -> [D.CanonicalDecl] -> [V.CanonicalDef]
toExpr moduleName = concatMap (toDefs moduleName)

toDefs :: String -> D.CanonicalDecl -> [V.CanonicalDef]
toDefs moduleName decl =
  let typeVar = Var.Canonical (Var.Module moduleName) in
  case decl of
    D.Definition def -> [def]

    D.Datatype name tvars constructors -> concatMap toDefs' constructors
      where
        toDefs' (ctor, tipes) =
            let vars = take (length tipes) arguments
                tbody = T.App (T.Type (typeVar name)) (map T.Var tvars)
                body = A.none . E.Data ctor $ map (A.none . E.localVar) vars
            in  [ definition ctor (buildFunction body vars) (foldr T.Lambda tbody tipes) ]

    D.TypeAlias name _ tipe@(T.Record fields ext) ->
        [ definition name (buildFunction record vars) (foldr T.Lambda result args) ]
      where
        result = T.Aliased (typeVar name) tipe

        args = map snd fields ++ maybe [] (:[]) ext

        var = A.none . E.localVar
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record = case ext of
                   Nothing -> A.none $ E.Record efields
                   Just _ -> foldl (\r (f,v) -> A.none $ E.Insert r f v) (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    D.TypeAlias _ _ _ -> []

    D.Port port ->
        case port of
          D.Out name expr@(A.A s _) tipe ->
              [ definition name (A.A s $ E.PortOut name tipe expr) tipe ]
          D.In name tipe ->
              [ definition name (A.none $ E.PortIn name tipe) tipe ]

    -- no constraints are needed for fixity declarations
    D.Fixity _ _ _ -> []


arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]

buildFunction :: V.CanonicalExpr -> [String] -> V.CanonicalExpr
buildFunction body@(A.A s _) vars =
    foldr (\p e -> A.A s (E.Lambda p e)) body (map P.Var vars)

definition :: String -> V.CanonicalExpr -> T.CanonicalType -> V.CanonicalDef
definition name expr tipe =
    V.Definition (P.Var name) expr (Just tipe)
