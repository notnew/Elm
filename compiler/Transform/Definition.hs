{-# OPTIONS_GHC -Wall #-}
module Transform.Definition where

import Control.Applicative ((<$>),(<*>))
import qualified AST.Pattern as P
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified Transform.Expression as Expr

validateExpr :: Source.Expr -> Either String Valid.RawExpr
validateExpr =
    Expr.crawlLet validateDefs validateCmds

validateDefs :: [Source.Def] -> Either String [Valid.RawDef]
validateDefs defs =
    case defs of
      Source.TypeAnnotation name tipe : rest ->
          case rest of
            Source.Definition pat@(P.Var name') expr : rest'
                | name == name' ->
                    do expr' <- validateExpr expr
                       let def = Valid.Definition pat expr' (Just tipe)
                       (:) def <$> validateDefs rest'

            _ -> Left $ "Formatting Error: The type annotation for '" ++ name ++
                        "' must be directly above its definition."

      Source.Definition pat expr : rest ->
          do expr' <- validateExpr expr
             let def = Valid.Definition pat expr' Nothing
             (:) def <$> validateDefs rest

      [] -> return []

validateCmds :: Source.Cmds -> Either String Valid.RawCmd
validateCmds (Source.Cmds cmds) = go cmds
  where
    go commands =
      case commands of
        Source.Do expr : [] ->
            Valid.Do <$> validateExpr expr

        Source.Do expr : cs ->
            Valid.DoAnd <$> validateExpr expr <*> go cs

        Source.CmdLet _defs : [] ->
            Left "Error: Command syntax cannot end with variable assignement.\n\
                 \It must end with an expression."

        Source.CmdLet defs : cs ->
            do mkCmd <- validateCmdDefs id [] defs
               mkCmd <$> go cs

        [] -> Left "Command syntax must have at least one command!"

validateCmdDefs :: (Valid.RawCmd -> Valid.RawCmd) -> [Valid.RawDef] -> [Source.CmdDef]
                -> Either String (Valid.RawCmd -> Valid.RawCmd)
validateCmdDefs mkCmd defs cmdDefs =
    case cmdDefs of
      Source.TypeAnn name tipe : rest ->
          case rest of
            Source.Assign  pat@(P.Var name') expr : rest' | name == name' ->
                assign pat expr (Just tipe) rest'

            Source.AndThen pat@(P.Var name') expr : rest' | name == name' ->
                andThen pat expr (Just tipe) rest'

            _ -> Left $ "Formatting Error: The type annotation for '" ++ name ++
                        "' must be directly above its definition in command syntax."

      Source.Assign pat expr : rest ->
          assign pat expr Nothing rest

      Source.AndThen pat expr : rest ->
          andThen pat expr Nothing rest

      [] -> return latestCmd

    where
      assign pat expr tipe rest = do
        expr' <- validateExpr expr
        let def = Valid.Definition pat expr' tipe
        validateCmdDefs mkCmd (def:defs) rest

      andThen pat expr tipe rest = do
        expr' <- validateExpr expr
        let mkCmd' = Valid.AndThen pat expr' tipe
        validateCmdDefs (latestCmd . mkCmd') [] rest

      latestCmd =
          case defs of
            [] -> mkCmd
            _  -> mkCmd . Valid.CmdLet defs
