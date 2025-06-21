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

bracketLeftToken :: ParsecT [Token] st IO Token
bracketLeftToken = tokenPrim show update_pos get_token
  where
    get_token BracketLeft = Just BracketLeft
    get_token _ = Nothing

bracketRightToken :: ParsecT [Token] st IO Token
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

notEqualToken :: ParsecT [Token] st IO Token
notEqualToken = tokenPrim show update_pos get_token
  where
    get_token NotEqual = Just NotEqual
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

typeToken :: ParsecT [Token] st IO Token
typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x) = Just (Type x)
    get_token _ = Nothing

idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show update_pos get_token
  where
    get_token (Id x) = Just (Id x)
    get_token _ = Nothing

floatToken :: ParsecT [Token] st IO Token
floatToken = tokenPrim show update_pos get_token
  where
    get_token (Float x) = Just (Float x)
    get_token _ = Nothing

intToken :: ParsecT [Token] st IO Token
intToken = tokenPrim show update_pos get_token
  where
    get_token (Int x) = Just (Int x)
    get_token _ = Nothing

stringToken :: ParsecT [Token] st IO Token
stringToken = tokenPrim show update_pos get_token
  where
    get_token (String x) = Just (String x)
    get_token _ = Nothing

boolToken :: ParsecT [Token] st IO Token
boolToken = tokenPrim show update_pos get_token
  where
    get_token (Bool x) = Just (Bool x)
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

vectorToken :: ParsecT [Token] st IO Token
vectorToken = tokenPrim show update_pos get_token
  where
    get_token Vector = Just Vector
    get_token _ = Nothing

matrixToken :: ParsecT [Token] st IO Token
matrixToken = tokenPrim show update_pos get_token
  where
    get_token Matrix = Just Matrix
    get_token _ = Nothing

printToken :: ParsecT [Token] st IO Token
printToken = tokenPrim show update_pos get_token
  where
    get_token Print = Just Print
    get_token _ = Nothing

scanToken :: ParsecT [Token] st IO Token
scanToken = tokenPrim show update_pos get_token
  where
    get_token Scan = Just Scan
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
  <|> try (do
    a <- user_defined_types_inicializations
    b <- initial_declarations
    return (a ++ b)
  )
  <|> try (do
    a <- data_structures_declarations
    b <- initial_declarations
    return (a ++ b)
  )
  <|> return []

data_structures_declarations :: ParsecT [Token] [(Token, Token)] IO [Token]
data_structures_declarations = do
  a <- idToken
  b <- matrixToken <|> vectorToken
  c <- lessToken
  d <- data_structures_contents
  e <- greaterToken
  h <- semiColonToken
  -- updateState (symtable_insert (a, get_default_value b))
  return ([a] ++ [b] ++ [c] ++ d ++ [e] ++ [h])

data_structures_contents :: ParsecT [Token] [(Token, Token)] IO [Token]
data_structures_contents =
  try (do
    a <- matrixToken <|> vectorToken
    b <- lessToken
    c <- data_structures_contents
    d <- greaterToken
    return ([a] ++ [b] ++ c ++ [d])
  )
  <|>
  try (do
    a <- try typeToken <|> try idToken
    return [a]
  )

user_defined_types :: ParsecT [Token] [(Token, Token)] IO [Token]
user_defined_types = enum <|> struct

user_defined_types_inicializations :: ParsecT [Token] [(Token, Token)] IO [Token]
user_defined_types_inicializations = do
  a <- idToken
  b <- idToken
  c <- semiColonToken
  return ([a] ++ [b] ++ [c])

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
  <|> try user_defined_types_inicializations
  <|> try data_structures_declarations
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
procedure_call =
  try (do
    a <- idToken
    b <- parenLeftToken
    c <- expressions <|> return []
    d <- parenRightToken
    e <- semiColonToken
    return ([a, b] ++ c ++ [d] ++ [e])
  )
  <|> try print_procedure

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

-- expressão geral
expression :: ParsecT [Token] [(Token, Token)] IO [Token]
expression =
  try arithmetic_expression
  <|> try boolean_expression
  <|> try string_tokens

-- expressões aritméticas
arithmetic_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
arithmetic_expression = add_sub_expression

-- soma e subtração – associatividade à esquerda
add_sub_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
add_sub_expression = chainl1 mul_div_rem_expression add_sub_op

-- multiplicação, divisão e resto – associatividade à esquerda
mul_div_rem_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
mul_div_rem_expression = chainl1 pow_expression mul_div_rem_op

-- potência – associatividade à direita
pow_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
pow_expression = chainr1 unary_expression pow_op

-- unário (ex: -x)
unary_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
unary_expression =
  try (do
    op <- unary_arithmetic_ops
    expr <- unary_expression
    return (op : expr)
  )
  <|> factor

-- fatores: unidades atômicas da expressão
factor :: ParsecT [Token] [(Token, Token)] IO [Token]
factor =
  try function_call
  <|> try data_structures_attribute_access
  <|> fmap (:[]) idToken
  <|> fmap (:[]) numeric_literal_tokens
  <|> 
  do
    a <- parenLeftToken
    b <- arithmetic_expression
    c <- parenRightToken
    return ([a] ++ b ++ [c])

-- operadores binários
add_sub_op :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
add_sub_op =
  (do 
    op <- addToken; 
    return (\a b -> a ++ [op] ++ b)
  )
  <|>
  (do 
    op <- subToken; 
    return (\a b -> a ++ [op] ++ b)
  )

mul_div_rem_op :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
mul_div_rem_op =
  (do 
    op <- mulToken; 
    return (\a b -> a ++ [op] ++ b)
  )
  <|>
  (do 
    op <- divToken; 
    return (\a b -> a ++ [op] ++ b)
  )
  <|>
  (do 
    op <- remToken; 
    return (\a b -> a ++ [op] ++ b)
  )

pow_op :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
pow_op = do
  op <- powToken
  return (\a b -> a ++ [op] ++ b)

-- operador unário
unary_arithmetic_ops :: ParsecT [Token] [(Token, Token)] IO Token
unary_arithmetic_ops = subToken










-- expressões booleanas gerais
boolean_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
boolean_expression = or_expression

-- OR (nível 0) – associatividade à esquerda
or_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
or_expression = chainl1 and_expression or_op

-- AND (nível 1) – associatividade à esquerda
and_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
and_expression = chainl1 boolean_base_expression and_op

-- operações unárias e comparações
boolean_base_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
boolean_base_expression =
  try unary_boolean_expression
  <|> try comparison_expression
  <|> 
  try (do
    a <- parenLeftToken
    b <- boolean_expression
    c <- parenRightToken
    return ([a] ++ b ++ [c])
  )

-- NOT (nível 2) – prefixado
unary_boolean_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
unary_boolean_expression = do
  op <- unary_boolean_ops
  expr <- boolean_expression
  return (op : expr)

comparison_expression :: ParsecT [Token] [(Token, Token)] IO [Token]
comparison_expression = do
  a <- expression
  op <- comparison_ops
  b <- expression
  return (a ++ [op] ++ b)

or_op :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
or_op = do
  op <- orToken
  return (\a b -> a ++ [op] ++ b)

and_op :: ParsecT [Token] [(Token, Token)] IO ([Token] -> [Token] -> [Token])
and_op = do
  op <- andToken
  return (\a b -> a ++ [op] ++ b)

comparison_ops :: ParsecT [Token] [(Token, Token)] IO Token
comparison_ops =
  try equalToken
  <|> try notEqualToken
  <|> try greaterToken
  <|> try greaterEqToken
  <|> try lessToken
  <|> try lessEqToken

unary_boolean_ops :: ParsecT [Token] [(Token, Token)] IO Token
unary_boolean_ops = try notToken














data_structures_attribute_access :: ParsecT [Token] [(Token, Token)] IO [Token]
data_structures_attribute_access =
  try user_defined_types_attribute_access
  <|> try vector_content_access
  <|> try matrix_content_access

user_defined_types_attribute_access :: ParsecT [Token] [(Token, Token)] IO [Token]
user_defined_types_attribute_access =
  try (do
    a <- idToken
    b <- dotToken
    c <- idToken
    return ([a, b] ++ [c])
  )

vector_content_access :: ParsecT [Token] [(Token, Token)] IO [Token]
vector_content_access = do
  a <- idToken
  b <- braceLeftToken
  c <- expression
  d <- braceRightToken
  return ([a, b] ++ c ++ [d])

matrix_content_access :: ParsecT [Token] [(Token, Token)] IO [Token]
matrix_content_access = do
  a <- idToken
  b <- braceLeftToken
  c <- expression
  d <- commaToken
  e <- expression
  f <- braceRightToken
  return ([a, b] ++ c ++ [d] ++ e ++ [f])

function_call :: ParsecT [Token] [(Token, Token)] IO [Token]
function_call =
  try (do
    a <- idToken
    b <- parenLeftToken
    c <- expressions <|> return []
    d <- parenRightToken
    return ([a, b] ++ c ++ [d])
  )
  <|> try scan_function

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
  j <- semiColonToken
  return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ [f] ++ g ++ [h] ++ [j])

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
  c <- boolean_expression
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

numeric_literal_tokens :: ParsecT [Token] [(Token, Token)] IO Token
numeric_literal_tokens =
  try intToken
  <|> try floatToken

string_tokens :: ParsecT [Token] [(Token, Token)] IO [Token]
string_tokens = do
  a <- stringToken
  return [a]

boolean_tokens :: ParsecT [Token] [(Token, Token)] IO [Token]
boolean_tokens = do
  a <- boolToken
  return [a]

print_procedure :: ParsecT [Token] [(Token, Token)] IO [Token]
print_procedure = do
  a <- printToken
  b <- parenLeftToken
  c <- expression <|> return []
  d <- parenRightToken
  e <- semiColonToken
  return ([a, b] ++ c ++ [d] ++ [e])

scan_function :: ParsecT [Token] [(Token, Token)] IO [Token]
scan_function = do
  a <- scanToken
  b <- parenLeftToken
  c <- parenRightToken
  return ([a] ++ [b] ++ [c])

-- inicialização de vetores e matrizes (vector<>, matrix<>) (tamanho dinâmico? como inicializar?)

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int") = Int 0

symtable_insert :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_insert symbol [] = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol] -- não detecta duplicatas

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
  tokens <- getTokens "programa.pe"
  result <- parser tokens
  case result of
    Left err -> print err
    Right ans -> print ans
