{-# OPTIONS_GHC -Wall #-}
module Transform.SortDefinitions (sortDefs) where

import Control.Monad.State
import Control.Applicative ((<$>),(<*>))
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import AST.Annotation
import AST.Expression.General (Expr'(..))
import qualified AST.Expression.Valid as V
import qualified AST.Pattern as P
import qualified AST.Variable as Var

ctors :: P.CanonicalPattern -> [String]
ctors pattern =
    case pattern of
      P.Var _ -> []
      P.Alias _ p -> ctors p
      P.Record _ -> []
      P.Anything -> []
      P.Literal _ -> []
      P.Data (Var.Canonical home name) ps ->
          case home of
            Var.Local -> name : rest
            Var.BuiltIn -> rest
            Var.Module _ -> rest
          where
            rest = concatMap ctors ps

free :: String -> State (Set.Set String) ()
free x = modify (Set.insert x)

freeIfLocal :: Var.Canonical -> State (Set.Set String) ()
freeIfLocal (Var.Canonical home name) =
    do case home of
         Var.Local -> free name
         Var.BuiltIn -> return ()
         Var.Module _ -> return ()

bound :: Set.Set String -> State (Set.Set String) ()
bound boundVars = modify (\freeVars -> Set.difference freeVars boundVars)

--restrictFreeVars :: P.Pattern -> State (Set.Set String) ()
restrictFreeVars :: P.CanonicalPattern -> State (Set.Set String) ()
restrictFreeVars pattern =
    do bound (P.boundVars pattern)
       mapM_ free (ctors pattern)

sortDefs :: V.CanonicalExpr -> V.CanonicalExpr
sortDefs expr = evalState (reorder expr) Set.empty

reorder :: V.CanonicalExpr -> State (Set.Set String) V.CanonicalExpr
reorder (A region expression) =
    A region <$>
    case expression of
      -- Be careful adding and restricting freeVars
      Var var -> freeIfLocal var >> return expression

      Lambda pattern expr ->
          do expr' <- reorder expr
             restrictFreeVars pattern
             return (Lambda pattern expr')

      Binop op e1 e2 ->
          do freeIfLocal op
             Binop op <$> reorder e1 <*> reorder e2

      Case expr cases ->
          do expr' <- reorder expr
             cases' <- forM cases $ \(p,e) -> do
                         e' <- reorder e
                         restrictFreeVars p
                         return (p, e')
             return (Case expr' cases')

      Data name es ->
          do free name
             Data name <$> mapM reorder es

      -- Just pipe the reorder though
      Literal _ -> return expression

      Range e1 e2 ->
          Range <$> reorder e1 <*> reorder e2

      ExplicitList es ->
          ExplicitList <$> mapM reorder es

      App e1 e2 ->
          App <$> reorder e1 <*> reorder e2

      MultiIf branches ->
          MultiIf <$> mapM (\(e1,e2) -> (,) <$> reorder e1 <*> reorder e2) branches

      Access e lbl ->
          Access <$> reorder e <*> return lbl

      Remove e lbl ->
          Remove <$> reorder e <*> return lbl

      Insert e lbl v ->
          Insert <$> reorder e <*> return lbl <*> reorder v

      Modify e fields ->
          Modify <$> reorder e <*> mapM (\(k,v) -> (,) k <$> reorder v) fields

      Record fields ->
          Record <$> mapM (\(k,v) -> (,) k <$> reorder v) fields

      Markdown uid md es -> Markdown uid md <$> mapM reorder es

      GLShader _ _ _ -> return expression

      PortOut name st signal -> PortOut name st <$> reorder signal

      PortIn name st -> return $ PortIn name st

      -- Actually do some reordering
      Let defs body ->
          do body' <- reorder body
             defss <- reorderDefs defs
             let A _ letBlock = foldr (\ds bod -> A region (Let ds bod)) body' defss
             return letBlock

      With impl cmd ->
          With <$> reorder impl <*> reorderCmd cmd

reorderDefs :: [V.CanonicalDef] -> State (Set.Set String) [[V.CanonicalDef]]
reorderDefs defs = do
  -- Sort defs into strongly connected components. This allows the programmer
  -- to write definitions in whatever order they please, we can still define
  -- things in order and generalize polymorphic functions when appropriate.
  sccs <- Graph.stronglyConnComp <$> buildDefDict defs
             
  -- remove let-bound variables from the context
  forM_ defs $ \(V.Definition p _ _) -> restrictFreeVars p

  return (map Graph.flattenSCC sccs)

reorderCmd :: V.CanonicalCmd -> State (Set.Set String) V.CanonicalCmd
reorderCmd command =
    case command of
      V.Do expr ->
          V.Do <$> reorder expr

      V.DoAnd expr cmd ->
          V.DoAnd <$> reorder expr <*> reorderCmd cmd

      V.CmdLet defs cmd ->
          do cmd' <- reorderCmd cmd
             defss <- reorderDefs defs
             return (foldr V.CmdLet cmd' defss)

      V.AndThen pattern expr tipe cmd ->
          do expr' <- reorder expr
             restrictFreeVars pattern
             V.AndThen pattern expr' tipe <$> reorderCmd cmd

reorderAndGetDependencies :: V.CanonicalDef
                          -> State (Set.Set String) (V.CanonicalDef, [String])
reorderAndGetDependencies (V.Definition pattern expr mType) =
    do globalFrees <- get
       -- work in a fresh environment
       put Set.empty
       expr' <- reorder expr
       localFrees <- get
       -- merge with global frees
       modify (Set.union globalFrees)
       return (V.Definition pattern expr' mType, Set.toList localFrees)


-- This also reorders the all of the sub-expressions in the Def list.
buildDefDict :: [V.CanonicalDef]
             -> State (Set.Set String) [(V.CanonicalDef, Int, [Int])]
buildDefDict defs =
  do pdefsDeps <- mapM reorderAndGetDependencies defs
     return $ realDeps (addKey pdefsDeps)

  where
--    addKey :: [(Canonical.Def, [String])] -> [(Canonical.Def, Int, [String])]
    addKey = zipWith (\n (pdef,deps) -> (pdef,n,deps)) [0..]

--    variableToKey :: (Canonical.Def, Int, [String]) -> [(String, Int)]
    variableToKey (V.Definition pattern _ _, key, _) =
        [ (var, key) | var <- Set.toList (P.boundVars pattern) ]

--    variableToKeyMap :: [(Canonical.Def, Int, [String])] -> Map.Map String Int
    variableToKeyMap pdefsDeps =
        Map.fromList (concatMap variableToKey pdefsDeps)

--    realDeps :: [(Canonical.Def, Int, [String])] -> [(Canonical.Def, Int, [Int])]
    realDeps pdefsDeps = map convert pdefsDeps
        where
          varDict = variableToKeyMap pdefsDeps
          convert (pdef, key, deps) =
              (pdef, key, Maybe.mapMaybe (flip Map.lookup varDict) deps)
