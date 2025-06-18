{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Main (main) where

import Control.Arrow (Arrow (first))
import Control.Monad (Monad (return))
import GHC.Float (divideDouble)
import GHC.IO.Device (RawIO (read))
import Lexer
import Text.Parsec
import Text.Parsec.Token (GenTokenParser (decimal))

-- parsers para os tokens

procedureToken = tokenPrim show update_pos get_token
  where
    get_token Procedure = Just Procedure
    get_token _ = Nothing

functionToken = tokenPrim show update_pos get_token
  where
    get_token Function = Just Function
    get_token _ = Nothing

commaToken = tokenPrim show update_pos get_token
  where
    get_token Comma = Just Comma
    get_token _ = Nothing

dotToken :: ParsecT [Token] st IO Token
dotToken = tokenPrim show update_pos get_token
  where
    get_token Dot = Just Dot
    get_token _ = Nothing

semiColonToken :: ParsecT [Token] st IO Token
semiColonToken = tokenPrim show update_pos get_token
  where
    get_token SemiColon = Just SemiColon
    get_token _ = Nothing

colonToken :: ParsecT [Token] st IO Token
colonToken = tokenPrim show update_pos get_token
  where
    get_token Colon = Just Colon
    get_token _ = Nothing

constToken :: ParsecT [Token] st IO Token
constToken = tokenPrim show update_pos get_token
  where
    get_token Const = Just Const
    get_token _ = Nothing

assignToken = tokenPrim show update_pos get_token
  where
    get_token Assign = Just Assign
    get_token _ = Nothing

addAssignToken :: ParsecT [Token] st IO Token
addAssignToken = tokenPrim show update_pos get_token
  where
    get_token AddAssign = Just AddAssign
    get_token _ = Nothing

subAssignToken :: ParsecT [Token] st IO Token
subAssignToken = tokenPrim show update_pos get_token
  where
    get_token SubAssign = Just SubAssign
    get_token _ = Nothing

mulAssignToken :: ParsecT [Token] st IO Token
mulAssignToken = tokenPrim show update_pos get_token
  where
    get_token MulAssign = Just MulAssign
    get_token _ = Nothing

divAssignToken :: ParsecT [Token] st IO Token
divAssignToken = tokenPrim show update_pos get_token
  where
    get_token DivAssign = Just DivAssign
    get_token _ = Nothing

remAssignToken :: ParsecT [Token] st IO Token
remAssignToken = tokenPrim show update_pos get_token
  where
    get_token RemAssign = Just RemAssign
    get_token _ = Nothing

powAssignToken :: ParsecT [Token] st IO Token
powAssignToken = tokenPrim show update_pos get_token
  where
    get_token PowAssign = Just PowAssign
    get_token _ = Nothing

addToken :: ParsecT [Token] st IO Token
addToken = tokenPrim show update_pos get_token
  where
    get_token Add = Just Add
    get_token _ = Nothing

subToken :: ParsecT [Token] st IO Token
subToken = tokenPrim show update_pos get_token
  where
    get_token Sub = Just Sub
    get_token _ = Nothing

mulToken :: ParsecT [Token] st IO Token
mulToken = tokenPrim show update_pos get_token
  where
    get_token Mul = Just Mul
    get_token _ = Nothing

divToken :: ParsecT [Token] st IO Token
divToken = tokenPrim show update_pos get_token
  where
    get_token Div = Just Div
    get_token _ = Nothing

remToken :: ParsecT [Token] st IO Token
remToken = tokenPrim show update_pos get_token
  where
    get_token Rem = Just Rem
    get_token _ = Nothing

powToken :: ParsecT [Token] st IO Token
powToken = tokenPrim show update_pos get_token
  where
    get_token Pow = Just Pow
    get_token _ = Nothing

andToken :: ParsecT [Token] st IO Token
andToken = tokenPrim show update_pos get_token
  where
    get_token And = Just And
    get_token _ = Nothing

orToken :: ParsecT [Token] st IO Token
orToken = tokenPrim show update_pos get_token
  where
    get_token Or = Just Or
    get_token _ = Nothing

notToken :: ParsecT [Token] st IO Token
notToken = tokenPrim show update_pos get_token
  where
    get_token Not = Just Not
    get_token _ = Nothing

bracketLeftToken = tokenPrim show update_pos get_token
  where
    get_token BracketLeft = Just BracketLeft
    get_token _ = Nothing

bracketRightToken = tokenPrim show update_pos get_token
  where
    get_token BracketRight = Just BracketRight
    get_token _ = Nothing

braceLeftToken :: ParsecT [Token] st IO Token
braceLeftToken = tokenPrim show update_pos get_token
  where
    get_token BraceLeft = Just BraceLeft
    get_token _ = Nothing

braceRightToken :: ParsecT [Token] st IO Token
braceRightToken = tokenPrim show update_pos get_token
  where
    get_token BraceRight = Just BraceRight
    get_token _ = Nothing

parenLeftToken :: ParsecT [Token] st IO Token
parenLeftToken = tokenPrim show update_pos get_token
  where
    get_token ParenLeft = Just ParenLeft
    get_token _ = Nothing

parenRightToken :: ParsecT [Token] st IO Token
parenRightToken = tokenPrim show update_pos get_token
  where
    get_token ParenRight = Just ParenRight
    get_token _ = Nothing

returnToken :: ParsecT [Token] st IO Token
returnToken = tokenPrim show update_pos get_token
  where
    get_token Return = Just Return
    get_token _ = Nothing

ifToken :: ParsecT [Token] st IO Token
ifToken = tokenPrim show update_pos get_token
  where
    get_token If = Just If
    get_token _ = Nothing

elseToken :: ParsecT [Token] st IO Token
elseToken = tokenPrim show update_pos get_token
  where
    get_token Else = Just Else
    get_token _ = Nothing

matchToken :: ParsecT [Token] st IO Token
matchToken = tokenPrim show update_pos get_token
  where
    get_token Match = Just Match
    get_token _ = Nothing

caseToken :: ParsecT [Token] st IO Token
caseToken = tokenPrim show update_pos get_token
  where
    get_token Case = Just Case
    get_token _ = Nothing

defaultToken :: ParsecT [Token] st IO Token
defaultToken = tokenPrim show update_pos get_token
  where
    get_token Default = Just Default
    get_token _ = Nothing

forToken :: ParsecT [Token] st IO Token
forToken = tokenPrim show update_pos get_token
  where
    get_token For = Just For
    get_token _ = Nothing

whileToken :: ParsecT [Token] st IO Token
whileToken = tokenPrim show update_pos get_token
  where
    get_token While = Just While
    get_token _ = Nothing

repeatToken :: ParsecT [Token] st IO Token
repeatToken = tokenPrim show update_pos get_token
  where
    get_token Repeat = Just Repeat
    get_token _ = Nothing

untilToken :: ParsecT [Token] st IO Token
untilToken = tokenPrim show update_pos get_token
  where
    get_token Until = Just Until
    get_token _ = Nothing

equalToken :: ParsecT [Token] st IO Token
equalToken = tokenPrim show update_pos get_token
  where
    get_token Equal = Just Equal
    get_token _ = Nothing

greaterToken :: ParsecT [Token] st IO Token
greaterToken = tokenPrim show update_pos get_token
  where
    get_token Greater = Just Greater
    get_token _ = Nothing

greaterEqToken :: ParsecT [Token] st IO Token
greaterEqToken = tokenPrim show update_pos get_token
  where
    get_token GreaterEq = Just GreaterEq
    get_token _ = Nothing

lessToken :: ParsecT [Token] st IO Token
lessToken = tokenPrim show update_pos get_token
  where
    get_token Less = Just Less
    get_token _ = Nothing

lessEqToken :: ParsecT [Token] st IO Token
lessEqToken = tokenPrim show update_pos get_token
  where
    get_token LessEq = Just LessEq
    get_token _ = Nothing

structToken :: ParsecT [Token] st IO Token
structToken = tokenPrim show update_pos get_token
  where
    get_token Struct = Just Struct
    get_token _ = Nothing

enumToken :: ParsecT [Token] st IO Token
enumToken = tokenPrim show update_pos get_token
  where
    get_token Enum = Just Enum
    get_token _ = Nothing

typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x) = Just (Type x)
    get_token _ = Nothing

idToken = tokenPrim show update_pos get_token
  where
    get_token (Id x) = Just (Id x)
    get_token _ = Nothing

floatToken :: ParsecT [Token] st IO Token
floatToken = tokenPrim show update_pos get_token
  where
    get_token (Float x) = Just (Float x)
    get_token _ = Nothing

intToken = tokenPrim show update_pos get_token
  where
    get_token (Int x) = Just (Int x)
    get_token _ = Nothing

stringToken :: ParsecT [Token] st IO Token
stringToken = tokenPrim show update_pos get_token
  where
    get_token (String x) = Just (String x)
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (next : _) = incSourceColumn pos 1 -- Avança um token
update_pos pos _ [] = pos -- Fim do código-fonte

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token, Token)] IO [Token]
program = do
  a <- declarations
  b <- subprograms
  return (a ++ b)

declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
declarations =
  try
    ( do
        d <- declaration
        rest <- declarations
        return (d ++ rest)
    )
    <|> ( do
            d <- declaration_assignment
            rest <- declarations
            return (d ++ rest)
        )
    <|> return []

declaration :: ParsecT [Token] [(Token, Token)] IO [Token]
declaration = do
  a <- idToken
  b <- typeToken
  c <- semiColonToken
  return ([a] ++ [b] ++ [c])

declaration_assignment :: ParsecT [Token] [(Token, Token)] IO [Token]
declaration_assignment = do
  a <- idToken
  b <- typeToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  return ([a] ++ [b] ++ [c] ++ d ++ [e])

m :: ParsecT [Token] [(Token, Token)] IO [Token]
m = procedure

subprograms :: ParsecT [Token] [(Token, Token)] IO [Token]
subprograms =
  ( do
      f <- function
      rest <- subprograms
      return (f ++ rest)
  )
    <|> ( do
            p <- procedure
            rest <- subprograms
            return (p ++ rest)
        )
    <|> return []

procedure :: ParsecT [Token] [(Token, Token)] IO [Token]
procedure = do
  a <- procedureToken
  b <- idToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- bracketLeftToken
  g <- stmts
  h <- bracketRightToken
  return (a : b : [c] ++ d ++ [e] ++ [f] ++ g ++ [h])

function :: ParsecT [Token] [(Token, Token)] IO [Token]
function = do
  a <- functionToken
  b <- idToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- typeToken
  g <- bracketLeftToken
  h <- stmts
  i <- bracketRightToken
  return (a : b : [c] ++ d ++ [e] ++ [f] ++ [g] ++ h ++ [i])

param :: ParsecT [Token] [(Token, Token)] IO [Token]
param = do
  a <- idToken
  b <- typeToken
  return (a : [b])

params :: ParsecT [Token] [(Token, Token)] IO [Token]
params = do
  first <- param
  next <- remainingParams
  return (first ++ next)

remainingParams :: ParsecT [Token] [(Token, Token)] IO [Token]
remainingParams =
  ( do
      a <- commaToken
      b <- param
      return (a : b)
  )
    <|> return []

varDecl :: ParsecT [Token] [(Token, Token)] IO [Token]
varDecl = do
  a <- idToken
  b <- typeToken
  updateState (symtable_insert (a, get_default_value b))
  return (a : [b])

stmts :: ParsecT [Token] [(Token, Token)] IO [Token]
stmts = do
  first <- assign
  next <- remaining_stmts
  return (first ++ next)

assign :: ParsecT [Token] [(Token, Token)] IO [Token]
assign = do
  a <- idToken
  b <- assignToken
  c <- intToken
  d <- semiColonToken
  updateState (symtable_update (a, c))
  return (a : b : [c])

remaining_stmts :: ParsecT [Token] [(Token, Token)] IO [Token]
remaining_stmts =
  ( do
      assign
  )
    <|> return []

expressions :: ParsecT [Token] [(Token, Token)] IO [Token]
expressions = do
  first <- expression
  next <- remaining_expressions
  return (first ++ next)

remaining_expressions :: ParsecT [Token] [(Token, Token)] IO [Token]
remaining_expressions =
  ( do
      a <- commaToken
      b <- expression
      return (a : b)
  )
    <|> return []

expression :: ParsecT [Token] [(Token, Token)] IO [Token]
expression =
  try function_call
    <|> chainl1 term addMinusOp

function_call :: ParsecT [Token] [(Token, Token)] IO [Token]
function_call = do
  a <- idToken
  b <- parenLeftToken
  c <- expressions
  d <- parenRightToken
  return ([a, b] ++ c ++ [d])

term :: ParsecT [Token] [(Token, Token)] IO [Token]
term = chainl1 (fmap (: []) factor) mulDivOp

addMinusOp :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
addMinusOp =
  (do op <- addToken; return (\a b -> a ++ [op] ++ b))
    <|> (do op <- divToken; return (\a b -> a ++ [op] ++ b))

mulDivOp :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
mulDivOp =
  (do op <- mulToken; return (\a b -> a ++ [op] ++ b))
    <|> (do op <- divToken; return (\a b -> a ++ [op] ++ b))

factor :: ParsecT [Token] [(Token, Token)] IO Token
factor = try idToken <|> try intToken <|> try floatToken

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int") = Int 0

symtable_insert :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_insert symbol [] = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol] -- Não detecta duplicatas.

symtable_update :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (id1, v1) ((id2, v2) : t) =
  if id1 == id2
    then (id1, v1) : t
    else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2) : t) =
  if id1 == id2
    then t
    else (id2, v2) : symtable_remove (id1, v1) t

-- invocação do parser para o símbolo de partida

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Parsing error!"

main :: IO ()
main = do
  tokens <- getTokens "programaV1V2.pe"
  result <- parser tokens
  case result of
    Left err -> print err
    Right ans -> print ans
