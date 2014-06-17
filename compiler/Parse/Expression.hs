module Parse.Expression (def,term,typeAnnotation,expr) where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent

import Parse.Binop
import Parse.Helpers as Help
import Parse.Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type

import AST.Annotation as Annotation
import AST.Expression.General
import qualified AST.Expression.Source as Source
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as Var


--------  Basic Terms  --------

varTerm :: IParser Source.Expr'
varTerm = toVar <$> var <?> "variable"

toVar :: String -> Source.Expr'
toVar v = case v of "True"  -> Literal (L.Boolean True)
                    "False" -> Literal (L.Boolean False)
                    _       -> rawVar v

accessor :: IParser Source.Expr'
accessor = do
  (start, lbl, end) <- located (try (string "." >> rLabel))
  let loc e = Annotation.at start end e
  return (Lambda (P.Var "_") (loc $ Access (loc $ rawVar "_") lbl))

negative :: IParser Source.Expr'
negative = do
  (start, nTerm, end) <-
      located (try (char '-' >> notFollowedBy (char '.' <|> char '-')) >> term)
  let loc e = Annotation.at start end e
  return (Binop (Var.Raw "-") (loc $ Literal (L.IntNum 0)) nTerm)


--------  Complex Terms  --------

listTerm :: IParser Source.Expr'
listTerm = markdown' <|> shader' <|> braces (try range <|> ExplicitList <$> commaSep expr)
  where
    range = do
      lo <- expr
      padded (string "..")
      Range lo <$> expr

    markdown' = do
      pos <- getPosition
      let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
      (rawText, exprs) <- Help.markdown (interpolation uid)
      return (Markdown uid (filter (/='\r') rawText) exprs)

    shader' = do
      pos <- getPosition
      let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
      (rawSrc, tipe) <- Help.shader
      return $ GLShader uid (filter (/='\r') rawSrc) tipe

    span uid index =
        "<span id=\"md-" ++ uid ++ "-" ++ show index ++
        "\">{{ markdown interpolation is in the pipeline, but still needs more testing }}</span>"

    interpolation uid exprs = do
      try (string "{{")
      e <- padded expr
      string "}}"
      return (span uid (length exprs), exprs ++ [e])

parensTerm :: IParser Source.Expr
parensTerm = try (parens opFn) <|> parens (tupleFn <|> parened)
  where
    opFn = do
      (start, op, end) <- located anyOp
      let loc = Annotation.at start end
      return . loc . Lambda (P.Var "x") . loc . Lambda (P.Var "y") . loc $
             Binop (Var.Raw op) (loc (rawVar "x")) (loc (rawVar "y"))

    tupleFn = do
      let comma = char ',' <?> "comma ','"
      (start, commas, end) <- located (comma >> many (whitespace >> comma))
      let vars = map (('v':) . show) [ 0 .. length commas + 1 ]
          loc = Annotation.at start end
      return $ foldr (\x e -> loc $ Lambda x e)
                 (loc . tuple $ map (loc . rawVar) vars) (map P.Var vars)
    
    parened = do
      (start, es, end) <- located (commaSep expr)
      return $ case es of
                 [e] -> e
                 _   -> Annotation.at start end (tuple es)

recordTerm :: IParser Source.Expr
recordTerm = brackets $ choice [ misc, addLocation record ]
    where
      field = do
        label <- rLabel
        patterns <- spacePrefix Pattern.term
        padded equals
        body <- expr
        return (label, makeFunction patterns body)
              
      record = Record <$> commaSep field

      change = do
        lbl <- rLabel
        padded (string "<-")
        (,) lbl <$> expr

      remove r = addLocation (string "-" >> whitespace >> Remove r <$> rLabel)

      insert r = addLocation $ do
                   string "|" >> whitespace
                   Insert r <$> rLabel <*> (padded equals >> expr)

      modify r =
          addLocation (string "|" >> whitespace >> Modify r <$> commaSep1 change)

      misc = try $ do
               record <- addLocation (rawVar <$> rLabel)
               opt <- padded (optionMaybe (remove record))
               case opt of
                 Just e  -> try (insert e) <|> return e
                 Nothing -> try (insert record) <|> try (modify record)
                        

term :: IParser Source.Expr
term =  addLocation (choice [ Literal <$> literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "basic term (4, x, 'c', etc.)"

--------  Applications  --------

appExpr :: IParser Source.Expr
appExpr = do
  t <- term
  ts <- constrainedSpacePrefix term $ \str ->
            if null str then notFollowedBy (char '-') else return ()
  return $ case ts of
             [] -> t
             _  -> foldl' (\f x -> Annotation.merge f x $ App f x) t ts

--------  Normal Expressions  --------

binaryExpr :: IParser Source.Expr
binaryExpr = binops appExpr lastExpr anyOp
  where lastExpr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
                <|> lambdaExpr

ifExpr :: IParser Source.Expr'
ifExpr = reserved "if" >> whitespace >> (normal <|> multiIf)
    where
      normal = do
        bool <- expr
        padded (reserved "then")
        thenBranch <- expr
        whitespace <?> "an 'else' branch" ; reserved "else" <?> "an 'else' branch" ; whitespace
        elseBranch <- expr
        return $ MultiIf [(bool, thenBranch),
                          (Annotation.sameAs elseBranch (Literal . L.Boolean $ True), elseBranch)]
      multiIf = MultiIf <$> spaceSep1 iff
          where iff = do string "|" ; whitespace
                         b <- expr ; padded arrow
                         (,) b <$> expr

lambdaExpr :: IParser Source.Expr
lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                args <- spaceSep1 Pattern.term
                padded arrow
                body <- expr
                return (makeFunction args body)

letExpr :: IParser Source.Expr'
letExpr = do
  reserved "let" ; whitespace
  defs <- blockOf def
  padded (reserved "in")
  Let defs <$> expr

caseExpr :: IParser Source.Expr'
caseExpr = do
  reserved "case"; e <- padded expr; reserved "of"; whitespace
  Case e <$> (with <|> without)
    where case_ = do p <- Pattern.expr
                     padded arrow
                     (,) p <$> expr
          with    = brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
          without = blockOf case_

withExpr :: IParser Source.Expr'
withExpr =
  do reserved "with"
     impl <- padded term
     cmd <- Source.Cmds <$> blockOf cmd
     return (With impl cmd)
  where
    cmd = choice [ Source.CmdLet <$> letBlock
                 , Source.Do <$> expr
                 ]

    letBlock = do
      try (reserved "let")
      whitespace
      blockOf cmdDef

    cmdDef = typeAnnotation Source.TypeAnn
          <|> definition (choice [ equals      >> return Source.Assign
                                 , string "<-" >> return Source.AndThen
                                 ])

expr :: IParser Source.Expr
expr = addLocation (choice [ ifExpr, letExpr, caseExpr, withExpr ])
    <|> lambdaExpr
    <|> binaryExpr 
    <?> "an expression"

defStart :: IParser [P.RawPattern]
defStart =
    choice [ do p1 <- try Pattern.term
                infics p1 <|> func p1
           , func =<< (P.Var <$> parens symOp)
           , (:[]) <$> Pattern.expr
           ] <?> "the definition of a variable (x = ...)"
    where
      func pattern =
          case pattern of
            P.Var _ -> (pattern:) <$> spacePrefix Pattern.term
            _ -> do try (lookAhead (whitespace >> string "="))
                    return [pattern]

      infics p1 = do
        o:p <- try (whitespace >> anyOp)
        p2  <- (whitespace >> Pattern.term)
        return $ if o == '`' then [ P.Var $ takeWhile (/='`') p, p1, p2 ]
                             else [ P.Var (o:p), p1, p2 ]

makeFunction :: [P.RawPattern] -> Source.Expr -> Source.Expr
makeFunction args body@(A ann _) =
    foldr (\arg body' -> A ann $ Lambda arg body') body args

definition :: IParser (P.RawPattern -> Source.Expr -> a) -> IParser a
definition equality = withPos $ do
  (name:args) <- defStart
  f <- padded equality
  body <- expr
  return $ f name (makeFunction args body)

typeAnnotation :: (String -> T.RawType -> a) -> IParser a
typeAnnotation f = f <$> try start <*> Type.expr
  where
    start = do
      v <- lowVar <|> parens symOp
      padded hasType
      return v

def :: IParser Source.Def
def = typeAnnotation Source.TypeAnnotation
   <|> definition (equals >> return Source.Definition)
