{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Main (main) where

import Control.Arrow (Arrow (first))
import Control.Monad (Monad (return))
import GHC.Float (divideDouble)
import GHC.IO.Device (RawIO (read))
import Lexer
import Text.Parsec
import Text.Parsec.Token (GenTokenParser (decimal, semi, comma))
import GHC.RTS.Flags (TraceFlags(user))

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

assignToken :: ParsecT [Token] st IO Token
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

trueToken :: ParsecT [Token] st IO Token
trueToken = tokenPrim show update_pos get_token
  where
    get_token TrueToken = Just TrueToken
    get_token _ = Nothing

falseToken :: ParsecT [Token] st IO Token
falseToken = tokenPrim show update_pos get_token
  where
    get_token FalseToken = Just FalseToken
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

guessToken :: ParsecT [Token] st IO Token
guessToken = tokenPrim show update_pos get_token
  where
    get_token Guess = Just Guess
    get_token _ = Nothing

mainToken :: ParsecT [Token] st IO Token
mainToken = tokenPrim show update_pos get_token
  where
    get_token Main = Just Main
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (next : _) = incSourceColumn pos 1 -- avança um token
update_pos pos _ [] = pos -- fim do código-fonte

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token, Token)] IO [Token]
program = do
  a <- initial_declarations
  b <- subprograms
  c <- m
  eof
  return (a ++ b ++ c)

initial_declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
initial_declarations =
  try (do
    a <- user_defined_types
    b <- initial_declarations
    return (a ++ b)
  )
  <|>
  try (do
    a <- variable_declarations
    b <- initial_declarations
    return (a ++ b)
  )
  <|> try (do
    a <- const_declarations
    b <- initial_declarations
    return (a ++ b)
  )
  <|> return []

user_defined_types :: ParsecT [Token] [(Token, Token)] IO [Token]
user_defined_types = enum <|> struct

struct :: ParsecT [Token] [(Token, Token)] IO [Token]
struct = do
    a <- structToken
    b <- idToken
    c <- bracketLeftToken
    d <- user_defined_types_declarations
    e <- bracketRightToken
    return ([a] ++ [b] ++ [c] ++ d ++ [e])

enum :: ParsecT [Token] [(Token, Token)] IO [Token]
enum = do
    a <- enumToken
    b <- idToken
    c <- bracketLeftToken
    d <- user_defined_types_declarations
    e <- bracketRightToken
    return ([a] ++ [b] ++ [c] ++ d ++ [e])

user_defined_types_declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
user_defined_types_declarations = do
  first <- try variable_declarations <|> try const_declarations
  next <- remaining_user_defined_types_declarations
  return (first ++ next)

remaining_user_defined_types_declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
remaining_user_defined_types_declarations = 
  (do
    first <- try variable_declarations <|> try const_declarations
    rest <- remaining_user_defined_types_declarations
    return (first ++ rest)
  )
  <|> return []
  
variable_declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
variable_declarations =
  try variable_declaration
  <|> try variable_declaration_assignment
  <|> try variable_guess_declaration_assignment

variable_declaration :: ParsecT [Token] [(Token, Token)] IO [Token]
variable_declaration = do
  a <- idToken
  b <- typeToken
  c <- semiColonToken
  updateState (symtable_insert (a, get_default_value b))
  return ([a] ++ [b] ++ [c])

variable_declaration_assignment :: ParsecT [Token] [(Token, Token)] IO [Token]
variable_declaration_assignment = do
  a <- idToken
  b <- typeToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  return ([a] ++ [b] ++ [c] ++ d ++ [e])

variable_guess_declaration_assignment :: ParsecT [Token] [(Token, Token)] IO [Token]
variable_guess_declaration_assignment = do
  a <- idToken
  b <- guessToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  --- updateState (symtable_insert (b, get_default_value c))
  return ([a] ++ [b] ++ [c] ++ d ++ [e])

const_declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
const_declarations =
  try const_declaration
  <|> try const_declaration_assignment
  <|> try const_guess_declaration_assignment

const_declaration :: ParsecT [Token] [(Token, Token)] IO [Token]
const_declaration = do
  a <- idToken
  b <- constToken
  c <- typeToken
  d <- semiColonToken
  return ([a] ++ [b] ++ [c] ++ [d])

const_declaration_assignment :: ParsecT [Token] [(Token, Token)] IO [Token]
const_declaration_assignment = do
  a <- idToken
  b <- constToken
  c <- typeToken
  d <- assignToken
  e <- expression
  f <- semiColonToken
  return ([a] ++ [b] ++ [c] ++ [d] ++ e ++ [f])

const_guess_declaration_assignment :: ParsecT [Token] [(Token, Token)] IO [Token]
const_guess_declaration_assignment = do
  a <- idToken
  b <- constToken
  c <- guessToken
  d <- assignToken
  e <- expression
  f <- semiColonToken
  return ([a] ++ [b] ++ [c] ++ [d] ++ e ++ [f])

m :: ParsecT [Token] [(Token, Token)] IO [Token]
m = do
  a <- procedureToken
  b <- mainToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- bracketLeftToken
  g <- stmts
  h <- bracketRightToken
  return (a : b : [c] ++ d ++ [e] ++ [f] ++ g ++ [h])

subprograms :: ParsecT [Token] [(Token, Token)] IO [Token]
subprograms =
  try (do
    f <- function
    rest <- subprograms
    return (f ++ rest)
  )
  <|> 
  try (do
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
params = 
  (do
    first <- param
    next <- remainingParams
    return (first ++ next)
  ) 
  <|> return []

remainingParams :: ParsecT [Token] [(Token, Token)] IO [Token]
remainingParams =
  (do
    a <- commaToken
    b <- param
    c <- remainingParams
    return (a : b ++ c)
  )
  <|> return []

stmts :: ParsecT [Token] [(Token, Token)] IO [Token]
stmts = do
  first <- stmt
  next <- remaining_stmts
  return (first ++ next)

remaining_stmts :: ParsecT [Token] [(Token, Token)] IO [Token]
remaining_stmts = 
  (do
    a <- stmt
    b <- remaining_stmts
    return (a ++ b)
  ) 
  <|> return []

stmt :: ParsecT [Token] [(Token, Token)] IO [Token]
stmt = 
  try loop 
  <|> try conditional 
  <|> try procedure_call 
  <|> try assign
  <|> try variable_declarations
  <|> try const_declarations
  <|> try function_return
  <|> try function_return

all_assign_tokens :: ParsecT [Token] [(Token, Token)] IO Token
all_assign_tokens =
  try addAssignToken
  <|> try subAssignToken
  <|> try mulAssignToken
  <|> try divAssignToken
  <|> try remAssignToken
  <|> try powAssignToken
  <|> try assignToken

assign :: ParsecT [Token] [(Token, Token)] IO [Token]
assign = do
  a <- idToken
  b <- all_assign_tokens
  c <- expression
  d <- semiColonToken
  -- updateState (symtable_update (a, c))
  return ([a] ++ [b] ++ c ++ [d])

procedure_call :: ParsecT [Token] [(Token, Token)] IO [Token]
procedure_call = do
  a <- idToken
  b <- parenLeftToken
  c <- expressions <|> return []
  d <- parenRightToken
  e <- semiColonToken
  return ([a, b] ++ c ++ [d] ++ [e])

function_return :: ParsecT [Token] [(Token, Token)] IO [Token]
function_return = do
  a <- returnToken
  b <- expression
  c <- semiColonToken
  return ([a] ++ b ++ [c])

expressions :: ParsecT [Token] [(Token, Token)] IO [Token]
expressions = do
  first <- expression
  next <- remaining_expressions
  return (first ++ next)

remaining_expressions :: ParsecT [Token] [(Token, Token)] IO [Token]
remaining_expressions =
  (do
    a <- commaToken
    b <- expression
    c <- remaining_expressions
    return (a : b ++ c)
  )
  <|> return []

expression :: ParsecT [Token] [(Token, Token)] IO [Token]
expression = chainl1 term addMinusOp

function_call :: ParsecT [Token] [(Token, Token)] IO [Token]
function_call = do
  a <- idToken
  b <- parenLeftToken
  c <- expressions <|> return []
  d <- parenRightToken
  return ([a, b] ++ c ++ [d])

term :: ParsecT [Token] [(Token, Token)] IO [Token]
term = chainl1 factor mulDivOp

addMinusOp :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
addMinusOp =
  (do 
    op <- addToken
    return (\a b -> a ++ [op] ++ b)
  )
  <|> 
  (do
    op <- divToken
    return (\a b -> a ++ [op] ++ b)
  )

mulDivOp :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
mulDivOp =
  (do 
    op <- mulToken 
    return (\a b -> a ++ [op] ++ b)
  )
  <|> 
  (do 
    op <- divToken 
    return (\a b -> a ++ [op] ++ b)
  )

factor :: ParsecT [Token] [(Token, Token)] IO [Token]
factor = 
  try function_call
  <|> fmap (: []) idToken
  <|> fmap (: []) intToken
  <|> fmap (: []) floatToken

loop :: ParsecT [Token] [(Token, Token)] IO [Token]
loop = while <|> repeat_until <|> for

while :: ParsecT [Token] [(Token, Token)] IO [Token]
while = do
  a <- whileToken
  b <- parenLeftToken
  c <- expression
  d <- parenRightToken
  e <- bracketLeftToken
  f <- stmts
  g <- bracketRightToken
  return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g])

repeat_until :: ParsecT [Token] [(Token, Token)] IO [Token]
repeat_until = do
  a <- repeatToken
  b <- bracketLeftToken
  c <- stmts
  d <- bracketRightToken
  e <- untilToken
  f <- parenLeftToken
  g <- expression
  h <- parenRightToken
  return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ [f] ++ g ++ [h])

for :: ParsecT [Token] [(Token, Token)] IO [Token]
for = do
  a <- forToken
  b <- parenLeftToken
  c <- variable_declaration
  d <- for_assign
  e <- semiColonToken
  f <- for_assign
  g <- parenRightToken
  h <- bracketLeftToken
  i <- stmts
  j <- bracketRightToken
  return ([a] ++ [b] ++ c ++ d ++ [e] ++ f ++ [g] ++ [h]++ i ++ [j])

for_assign :: ParsecT [Token] [(Token, Token)] IO [Token]
for_assign = do
  a <- idToken
  b <- all_assign_tokens
  c <- expression
  return ([a] ++ [b] ++ c)

conditional :: ParsecT [Token] [(Token, Token)] IO [Token]
conditional = if_else <|> match_case

if_else :: ParsecT [Token] [(Token, Token)] IO [Token]
if_else = do
    first <- cond_if
    next <- cond_else
    return (first ++ next)

cond_if :: ParsecT [Token] [(Token, Token)] IO [Token]
cond_if = do
  a <- ifToken
  b <- parenLeftToken
  c <- expression
  d <- parenRightToken
  e <- bracketLeftToken
  f <- stmts
  g <- bracketRightToken
  return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g])

cond_else :: ParsecT [Token] [(Token, Token)] IO [Token]
cond_else =
  (do
    a <- elseToken
    b <- bracketLeftToken
    c <- stmts
    d <- bracketRightToken
    return ([a] ++ [b] ++ c ++ [d])
  )
  <|>
  return []

match_case :: ParsecT [Token] [(Token, Token)] IO [Token]
match_case = do
  a <- matchToken
  b <- parenLeftToken
  c <- expression
  d <- parenRightToken
  e <- bracketLeftToken
  f <- cases
  g <- default_case
  h <- bracketRightToken
  return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ g ++ [h])

cases :: ParsecT [Token] [(Token, Token)] IO [Token]
cases =
  (do
  a <- caseToken
  b <- expression
  c <- colonToken
  d <- stmts
  return ([a] ++ b ++ [c] ++ d)
  )
  <|> return []

default_case :: ParsecT [Token] [(Token, Token)] IO [Token]
default_case = do
  a <- defaultToken
  b <- colonToken
  c <- stmts
  return (a : b : c)

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
