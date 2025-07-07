{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main (main) where

import Control.Arrow (Arrow (first))
import Control.Monad (Functor (fmap), Monad (return), when)
import Control.Monad.IO.Class
import GHC.Float (divideDouble)
import GHC.RTS.Flags (TraceFlags (user))
import Lexer
import Text.Parsec
import Data.Char (toLower)
import Distribution.Compat.Prelude (unless)

-- nome, tipo, escopo, tempo de vida, valor, endereço
-- nome -> tabela de simbolos junto com escopo escopo#nome
-- tipo e valor -> unificados
--
-- runtime stack
--
-- pilha dos escopos
--
-- lista de tipos criados por usuário
--
-- tabela de subprogramas declarados
-- nome, parametros, tipo de retorno e codigo
-- data SubP = Proc ... | Func ...
--
-- flag se está executando
-- falsa se está em declaração de subprograma
-- verdadeira quando entra na main

type SymbolTable = [(String, Type)]

data Type = Integer Int | Floating Float | Str String | Boolean Bool | Record String [(String, Type)]
    deriving (Show, Eq)

data MemoryState = MemoryState {
  symtable :: SymbolTable,
  typetable :: [Type],
  executing :: Bool
}

-- parsers para os tokens

procedureToken :: ParsecT [Token] st IO Token
procedureToken = tokenPrim show update_pos get_token
  where
    get_token (Procedure p) = Just (Procedure p)
    get_token _ = Nothing

functionToken :: ParsecT [Token] st IO Token
functionToken = tokenPrim show update_pos get_token
  where
    get_token (Function p) = Just (Function p)
    get_token _ = Nothing

commaToken :: ParsecT [Token] st IO Token
commaToken = tokenPrim show update_pos get_token
  where
    get_token (Comma p) = Just (Comma p)
    get_token _ = Nothing

dotToken :: ParsecT [Token] st IO Token
dotToken = tokenPrim show update_pos get_token
  where
    get_token (Dot p) = Just (Dot p)
    get_token _ = Nothing

semiColonToken :: ParsecT [Token] st IO Token
semiColonToken = tokenPrim show update_pos get_token
  where
    get_token (SemiColon p) = Just (SemiColon p)
    get_token _ = Nothing

colonToken :: ParsecT [Token] st IO Token
colonToken = tokenPrim show update_pos get_token
  where
    get_token (Colon p) = Just (Colon p)
    get_token _ = Nothing

constToken :: ParsecT [Token] st IO Token
constToken = tokenPrim show update_pos get_token
  where
    get_token (Const p) = Just (Const p)
    get_token _ = Nothing

assignToken :: ParsecT [Token] st IO Token
assignToken = tokenPrim show update_pos get_token
  where
    get_token (Assign p) = Just (Assign p)
    get_token _ = Nothing

addAssignToken :: ParsecT [Token] st IO Token
addAssignToken = tokenPrim show update_pos get_token
  where
    get_token (AddAssign p) = Just (AddAssign p)
    get_token _ = Nothing

subAssignToken :: ParsecT [Token] st IO Token
subAssignToken = tokenPrim show update_pos get_token
  where
    get_token (SubAssign p) = Just (SubAssign p)
    get_token _ = Nothing

mulAssignToken :: ParsecT [Token] st IO Token
mulAssignToken = tokenPrim show update_pos get_token
  where
    get_token (MulAssign p) = Just (MulAssign p)
    get_token _ = Nothing

divAssignToken :: ParsecT [Token] st IO Token
divAssignToken = tokenPrim show update_pos get_token
  where
    get_token (DivAssign p) = Just (DivAssign p)
    get_token _ = Nothing

remAssignToken :: ParsecT [Token] st IO Token
remAssignToken = tokenPrim show update_pos get_token
  where
    get_token (RemAssign p) = Just (RemAssign p)
    get_token _ = Nothing

powAssignToken :: ParsecT [Token] st IO Token
powAssignToken = tokenPrim show update_pos get_token
  where
    get_token (PowAssign p) = Just (PowAssign p)
    get_token _ = Nothing

addToken :: ParsecT [Token] st IO Token
addToken = tokenPrim show update_pos get_token
  where
    get_token (Add p) = Just (Add p)
    get_token _ = Nothing

subToken :: ParsecT [Token] st IO Token
subToken = tokenPrim show update_pos get_token
  where
    get_token (Sub p) = Just (Sub p)
    get_token _ = Nothing

mulToken :: ParsecT [Token] st IO Token
mulToken = tokenPrim show update_pos get_token
  where
    get_token (Mul p) = Just (Mul p)
    get_token _ = Nothing

divToken :: ParsecT [Token] st IO Token
divToken = tokenPrim show update_pos get_token
  where
    get_token (Div p) = Just (Div p)
    get_token _ = Nothing

remToken :: ParsecT [Token] st IO Token
remToken = tokenPrim show update_pos get_token
  where
    get_token (Rem p) = Just (Rem p)
    get_token _ = Nothing

powToken :: ParsecT [Token] st IO Token
powToken = tokenPrim show update_pos get_token
  where
    get_token (Pow p) = Just (Pow p)
    get_token _ = Nothing

andToken :: ParsecT [Token] st IO Token
andToken = tokenPrim show update_pos get_token
  where
    get_token (And p) = Just (And p)
    get_token _ = Nothing

orToken :: ParsecT [Token] st IO Token
orToken = tokenPrim show update_pos get_token
  where
    get_token (Or p) = Just (Or p)
    get_token _ = Nothing

notToken :: ParsecT [Token] st IO Token
notToken = tokenPrim show update_pos get_token
  where
    get_token (Not p) = Just (Not p)
    get_token _ = Nothing

bracketLeftToken :: ParsecT [Token] st IO Token
bracketLeftToken = tokenPrim show update_pos get_token
  where
    get_token (BracketLeft p) = Just (BracketLeft p)
    get_token _ = Nothing

bracketRightToken :: ParsecT [Token] st IO Token
bracketRightToken = tokenPrim show update_pos get_token
  where
    get_token (BracketRight p) = Just (BracketRight p)
    get_token _ = Nothing

braceLeftToken :: ParsecT [Token] st IO Token
braceLeftToken = tokenPrim show update_pos get_token
  where
    get_token (BraceLeft p) = Just (BraceLeft p)
    get_token _ = Nothing

braceRightToken :: ParsecT [Token] st IO Token
braceRightToken = tokenPrim show update_pos get_token
  where
    get_token (BraceRight p) = Just (BraceRight p)
    get_token _ = Nothing

parenLeftToken :: ParsecT [Token] st IO Token
parenLeftToken = tokenPrim show update_pos get_token
  where
    get_token (ParenLeft p) = Just (ParenLeft p)
    get_token _ = Nothing

parenRightToken :: ParsecT [Token] st IO Token
parenRightToken = tokenPrim show update_pos get_token
  where
    get_token (ParenRight p) = Just (ParenRight p)
    get_token _ = Nothing

returnToken :: ParsecT [Token] st IO Token
returnToken = tokenPrim show update_pos get_token
  where
    get_token (Return p) = Just (Return p)
    get_token _ = Nothing

ifToken :: ParsecT [Token] st IO Token
ifToken = tokenPrim show update_pos get_token
  where
    get_token (If p) = Just (If p)
    get_token _ = Nothing

elseToken :: ParsecT [Token] st IO Token
elseToken = tokenPrim show update_pos get_token
  where
    get_token (Else p) = Just (Else p)
    get_token _ = Nothing

matchToken :: ParsecT [Token] st IO Token
matchToken = tokenPrim show update_pos get_token
  where
    get_token (Match p) = Just (Match p)
    get_token _ = Nothing

caseToken :: ParsecT [Token] st IO Token
caseToken = tokenPrim show update_pos get_token
  where
    get_token (Case p) = Just (Case p)
    get_token _ = Nothing

defaultToken :: ParsecT [Token] st IO Token
defaultToken = tokenPrim show update_pos get_token
  where
    get_token (Default p) = Just (Default p)
    get_token _ = Nothing

forToken :: ParsecT [Token] st IO Token
forToken = tokenPrim show update_pos get_token
  where
    get_token (For p) = Just (For p)
    get_token _ = Nothing

whileToken :: ParsecT [Token] st IO Token
whileToken = tokenPrim show update_pos get_token
  where
    get_token (While p) = Just (While p)
    get_token _ = Nothing

repeatToken :: ParsecT [Token] st IO Token
repeatToken = tokenPrim show update_pos get_token
  where
    get_token (Repeat p) = Just (Repeat p)
    get_token _ = Nothing

untilToken :: ParsecT [Token] st IO Token
untilToken = tokenPrim show update_pos get_token
  where
    get_token (Until p) = Just (Until p)
    get_token _ = Nothing

equalToken :: ParsecT [Token] st IO Token
equalToken = tokenPrim show update_pos get_token
  where
    get_token (Equal p) = Just (Equal p)
    get_token _ = Nothing

notEqualToken :: ParsecT [Token] st IO Token
notEqualToken = tokenPrim show update_pos get_token
  where
    get_token (NotEqual p) = Just (NotEqual p)
    get_token _ = Nothing

greaterToken :: ParsecT [Token] st IO Token
greaterToken = tokenPrim show update_pos get_token
  where
    get_token (Greater p) = Just (Greater p)
    get_token _ = Nothing

greaterEqToken :: ParsecT [Token] st IO Token
greaterEqToken = tokenPrim show update_pos get_token
  where
    get_token (GreaterEq p) = Just (GreaterEq p)
    get_token _ = Nothing

lessToken :: ParsecT [Token] st IO Token
lessToken = tokenPrim show update_pos get_token
  where
    get_token (Less p) = Just (Less p)
    get_token _ = Nothing

lessEqToken :: ParsecT [Token] st IO Token
lessEqToken = tokenPrim show update_pos get_token
  where
    get_token (LessEq p) = Just (LessEq p)
    get_token _ = Nothing

structToken :: ParsecT [Token] st IO Token
structToken = tokenPrim show update_pos get_token
  where
    get_token (Struct p) = Just (Struct p)
    get_token _ = Nothing

enumToken :: ParsecT [Token] st IO Token
enumToken = tokenPrim show update_pos get_token
  where
    get_token (Enum p) = Just (Enum p)
    get_token _ = Nothing

typeToken :: ParsecT [Token] st IO Token
typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x p) = Just (Type x p)
    get_token _ = Nothing

idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show update_pos get_token
  where
    get_token (Id x p) = Just (Id x p)
    get_token _ = Nothing

floatToken :: ParsecT [Token] st IO Token
floatToken = tokenPrim show update_pos get_token
  where
    get_token (Float x p) = Just (Float x p)
    get_token _ = Nothing

intToken :: ParsecT [Token] st IO Token
intToken = tokenPrim show update_pos get_token
  where
    get_token (Int x p) = Just (Int x p)
    get_token _ = Nothing

stringToken :: ParsecT [Token] st IO Token
stringToken = tokenPrim show update_pos get_token
  where
    get_token (String x p) = Just (String x p)
    get_token _ = Nothing

boolToken :: ParsecT [Token] st IO Token
boolToken = tokenPrim show update_pos get_token
  where
    get_token (Bool x p) = Just (Bool x p)
    get_token _ = Nothing

guessToken :: ParsecT [Token] st IO Token
guessToken = tokenPrim show update_pos get_token
  where
    get_token (Guess p) = Just (Guess p)
    get_token _ = Nothing

mainToken :: ParsecT [Token] st IO Token
mainToken = tokenPrim show update_pos get_token
  where
    get_token (Main p) = Just (Main p)
    get_token _ = Nothing

vectorToken :: ParsecT [Token] st IO Token
vectorToken = tokenPrim show update_pos get_token
  where
    get_token (Vector p) = Just (Vector p)
    get_token _ = Nothing

matrixToken :: ParsecT [Token] st IO Token
matrixToken = tokenPrim show update_pos get_token
  where
    get_token (Matrix p) = Just (Matrix p)
    get_token _ = Nothing

printToken :: ParsecT [Token] st IO Token
printToken = tokenPrim show update_pos get_token
  where
    get_token (Print p) = Just (Print p)
    get_token _ = Nothing

scanToken :: ParsecT [Token] st IO Token
scanToken = tokenPrim show update_pos get_token
  where
    get_token (Scan p) = Just (Scan p)
    get_token _ = Nothing

continueToken :: ParsecT [Token] st IO Token
continueToken = tokenPrim show update_pos get_token
  where
    get_token (Continue p) = Just (Continue p)
    get_token _ = Nothing

leaveToken :: ParsecT [Token] st IO Token
leaveToken = tokenPrim show update_pos get_token
  where
    get_token (Leave p) = Just (Leave p)
    get_token _ = Nothing

breakToken :: ParsecT [Token] st IO Token
breakToken = tokenPrim show update_pos get_token
  where
    get_token (Break p) = Just (Break p)
    get_token _ = Nothing

nullToken :: ParsecT [Token] st IO Token
nullToken = tokenPrim show update_pos get_token
  where
    get_token (Null p) = Just (Null p)
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (next : _) = incSourceColumn pos 1 -- avança um token
update_pos pos _ [] = pos -- fim do código-fonte

-- parsers para os não-terminais

program :: ParsecT [Token] MemoryState IO ()
program = do
  a <- initial_declarations
  b <- m
  -- print_symtable
  -- print_types
  eof
  return ()

initial_declarations :: ParsecT [Token] MemoryState IO [Token]
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
    return b
    -- return (a ++ b)
  )
  <|> try (do
    a <- const_declarations
    b <- initial_declarations
    return (a ++ b)
  )
  <|> try (do
    a <- data_structures_declarations
    b <- initial_declarations
    return (a ++ b)
  )
  <|> try (do
    a <- subprograms
    b <- initial_declarations
    return (a ++ b)
  )
  <|> return []

data_structures_declarations :: ParsecT [Token] MemoryState IO [Token]
data_structures_declarations = do
  a <- idToken
  b <- matrixToken <|> vectorToken
  c <- lessToken
  d <- all_possible_type_tokens
  e <- greaterToken
  h <- semiColonToken
  -- updateState (symtable_insert (a, get_default_value b))
  return [a]
  -- return ([a] ++ [b] ++ [c] ++ d ++ [e] ++ [h])

user_defined_types :: ParsecT [Token] MemoryState IO [Token]
user_defined_types = enum <|> struct

struct :: ParsecT [Token] MemoryState IO [Token]
struct = do
    a <- structToken
    b <- idToken
    c <- bracketLeftToken
    d <- user_defined_types_declarations
    e <- bracketRightToken
    let Id b_name _ = b in do
        updateState (typetable_insert $ Record b_name d)
        return ([a] ++ [b] ++ [c] ++ [e])

enum :: ParsecT [Token] MemoryState IO [Token]
enum = do
    a <- enumToken
    b <- idToken
    c <- bracketLeftToken
    d <- user_defined_types_declarations
    e <- bracketRightToken
    return ([a] ++ [b] ++ [c] ++ [e])
    -- return ([a] ++ [b] ++ [c] ++ d ++ [e])

user_defined_types_declarations :: ParsecT [Token] MemoryState IO [(String, Type)]
user_defined_types_declarations = do
  first <- try variable_declarations -- <|> try const_declarations
  next <- remaining_user_defined_types_declarations
  return (first : next)

remaining_user_defined_types_declarations :: ParsecT [Token] MemoryState IO [(String, Type)]
remaining_user_defined_types_declarations =
  (do
    first <- try variable_declarations -- <|> try const_declarations
    rest <- remaining_user_defined_types_declarations
    return (first : rest)
  )
  <|> return []

variable_declarations :: ParsecT [Token] MemoryState IO (String, Type)
variable_declarations =
  try variable_declaration
  -- <|> try variable_declaration_assignment
  -- <|> try variable_guess_declaration_assignment

variable_declaration :: ParsecT [Token] MemoryState IO (String, Type)
variable_declaration = do
  a <- idToken
  -- b <- typeToken
  b <- all_possible_type_tokens
  c <- semiColonToken
  s <- getState
  when (executing s) $
    updateState (symtable_insert (a, b))
  let Id a_name _ = a in
    return (a_name, b)
  -- return ([a] ++ b ++ [c])

variable_declaration_assignment :: ParsecT [Token] MemoryState IO [Token]
variable_declaration_assignment = do
  a <- idToken
  b <- typeToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  return [a]
  -- return ([a] ++ [b] ++ [c] ++ d ++ [e])

variable_guess_declaration_assignment :: ParsecT [Token] MemoryState IO [Token]
variable_guess_declaration_assignment = do
  a <- idToken
  b <- guessToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  --- updateState (symtable_insert (b, get_default_value c))
  return [a]
  -- return ([a] ++ [b] ++ [c] ++ d ++ [e])

const_declarations :: ParsecT [Token] MemoryState IO [Token]
const_declarations =
  try const_declaration
  <|> try const_declaration_assignment
  <|> try const_guess_declaration_assignment

const_declaration :: ParsecT [Token] MemoryState IO [Token]
const_declaration = do
  a <- idToken
  b <- constToken
  c <- typeToken
  d <- semiColonToken
  return ([a] ++ [b] ++ [c] ++ [d])

const_declaration_assignment :: ParsecT [Token] MemoryState IO [Token]
const_declaration_assignment = do
  a <- idToken
  b <- constToken
  c <- typeToken
  d <- assignToken
  e <- expression
  f <- semiColonToken
  return [a]
  -- return ([a] ++ [b] ++ [c] ++ [d] ++ e ++ [f])

const_guess_declaration_assignment :: ParsecT [Token] MemoryState IO [Token]
const_guess_declaration_assignment = do
  a <- idToken
  b <- constToken
  c <- guessToken
  d <- assignToken
  e <- expression
  f <- semiColonToken
  return [a]
  -- return ([a] ++ [b] ++ [c] ++ [d] ++ e ++ [f])

m :: ParsecT [Token] MemoryState IO [Token]
m = do
  s <- getState
  putState s {executing = True}
  a <- procedureToken
  b <- mainToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- bracketLeftToken
  g <- stmts
  h <- bracketRightToken
  return [a]
  -- return (a : b : [c] ++ d ++ [e] ++ [f] ++ g ++ [h])

subprograms :: ParsecT [Token] MemoryState IO [Token]
subprograms = try function <|> try procedure

procedure :: ParsecT [Token] MemoryState IO [Token]
procedure = do
  a <- procedureToken
  b <- idToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- bracketLeftToken
  g <- stmts
  h <- bracketRightToken
  return [a]
  -- return (a : b : [c] ++ d ++ [e] ++ [f] ++ g ++ [h])

function :: ParsecT [Token] MemoryState IO [Token]
function = do
  a <- functionToken
  b <- idToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- all_possible_type_tokens
  g <- bracketLeftToken
  h <- stmts
  i <- bracketRightToken
  return [a]
  -- return (a : b : [c] ++ d ++ [e] ++ f ++ [g] ++ h ++ [i])

param :: ParsecT [Token] MemoryState IO (String, Type)
param = do
  a <- idToken
  b <- all_possible_type_tokens
  let Id a_name _ = a in
    return (a_name, b)

params :: ParsecT [Token] MemoryState IO [(String, Type)]
params =
  (do
    first <- param
    next <- remainingParams
    return (first : next)
  )
  <|> return []

remainingParams :: ParsecT [Token] MemoryState IO [(String, Type)]
remainingParams =
  (do
    a <- commaToken
    b <- param
    c <- remainingParams
    return (b : c)
  )
  <|> return []

stmts :: ParsecT [Token] MemoryState IO [Token]
stmts = do
  first <- stmt
  next <- remaining_stmts
  return (first ++ next)

remaining_stmts :: ParsecT [Token] MemoryState IO [Token]
remaining_stmts =
  (do
    a <- stmt
    b <- remaining_stmts
    return (a ++ b)
  )
  <|> return []

stmt :: ParsecT [Token] MemoryState IO [Token]
stmt =
  try loop
  <|> try all_escapes_stmts
  <|> try conditional
  <|> try procedure_call
  <|> try assign
  <|> do
    try variable_declarations
    return []
  <|> try const_declarations
  <|> try data_structures_declarations
  <|> try function_return

all_assign_tokens :: ParsecT [Token] MemoryState IO (Token -> Type -> MemoryState -> Either String Type)
all_assign_tokens =
  try (do
    Assign pos@(line, col) <- assignToken
    return (assign_eq (line, col))
  )
  <|>
  try (do
    AddAssign pos@(line, col) <- addAssignToken
    return (assign_add (line, col))
  )
  <|>
  try (do
    SubAssign pos@(line, col) <- subAssignToken
    return (assign_sub (line, col))
  )
  <|>
  try (do
    MulAssign pos@(line, col) <- mulAssignToken
    return (assign_mul (line, col))
  )
  <|>
  try (do
    DivAssign pos@(line, col) <- divAssignToken
    return (assign_div (line, col))
  )
  <|>
  try (do
    RemAssign pos@(line, col) <- remAssignToken
    return (assign_rem (line, col))
  )
  <|>
  try (do
    PowAssign pos@(line, col) <- powAssignToken
    return (assign_pow (line, col))
  )

assign_eq :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_eq = assign_with_op (\_ _ expr -> expr)

assign_add :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_add = assign_with_op do_number_add_op

assign_sub :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_sub = assign_with_op do_sub_op

assign_mul :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_mul = assign_with_op do_mul_op

assign_div :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_div = assign_with_op do_div_op

assign_rem :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_rem = assign_with_op do_rem_op

assign_pow :: (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_pow = assign_with_op do_pow_op

assign_with_op :: ((Int, Int) -> (Type -> Type -> Type)) -> (Int, Int) -> (Token -> Type -> MemoryState -> Either String Type)
assign_with_op op (line, column) variable_name expression state =
  let
    (Id name _) = variable_name
    variable_type = get_variable_value variable_name state
    base_variable_type = extract_base_type variable_type
    base_expr_type = extract_base_type expression
    result = op (line, column) variable_type expression
  in
    if compare_type_base base_variable_type base_expr_type then
      Right result
    else
      Left $ "type error: variable " ++ show name ++ " expects type " ++ show_pretty_types base_variable_type ++
             ", but got " ++ show_pretty_type_values result ++
             "; line: " ++ show line ++ ", column: " ++ show column

assign :: ParsecT [Token] MemoryState IO [Token]
assign = try $ do
  a <- idToken
  assign_function <- all_assign_tokens
  b <- expression
  c <- semiColonToken
  s <- getState
  when (executing s) $ do
    case assign_function a b s of
      Right new_value -> updateState (symtable_update (a, new_value))
      Left error_msg    -> fail error_msg
  return [a]
  <|>
  try (do
    a <- idToken
    b <- all_assign_tokens
    c <- expression
    d <- semiColonToken
    -- updateState (symtable_update (a, c))
    return [a]
    -- return ([a] ++ [b] ++ c ++ [d])
  )

procedure_call :: ParsecT [Token] MemoryState IO [Token]
procedure_call =
  try (do
    a <- idToken
    b <- parenLeftToken
    c <- expressions <|> return []
    d <- parenRightToken
    e <- semiColonToken
    return [a]
    -- return ([a, b] ++ c ++ [d] ++ [e])
  )
  <|> try print_procedure

function_return :: ParsecT [Token] MemoryState IO [Token]
function_return = do
  a <- returnToken
  b <- expression
  c <- semiColonToken
  return [a]
  -- return ([a] ++ b ++ [c])

expressions :: ParsecT [Token] MemoryState IO [Type]
expressions = do
  first <- expression
  next <- remaining_expressions
  return (first : next)

remaining_expressions :: ParsecT [Token] MemoryState IO [Type]
remaining_expressions =
  (do
    a <- commaToken
    b <- expression
    c <- remaining_expressions
    return (b : c)
  )
  <|> return []

-- expressão geral (começa pelo or, que tem menor prioridade)
expression :: ParsecT [Token] MemoryState IO Type
expression = or_expression

-- or lógico – associatividade à esquerda
or_expression :: ParsecT [Token] MemoryState IO Type
or_expression = chainl1 and_expression or_op

-- and lógico – associatividade à esquerda
and_expression :: ParsecT [Token] MemoryState IO Type
and_expression = chainl1 equal_different_expression and_op

-- igual ou diferente – associatividade à esquerda
equal_different_expression :: ParsecT [Token] MemoryState IO Type
equal_different_expression = chainl1 greater_lesser_expression equal_different_op

-- maior/igual que e menor/igual que – associatividade à esquerda
greater_lesser_expression :: ParsecT [Token] MemoryState IO Type
greater_lesser_expression = chainl1 add_sub_expression greater_lesser_op

-- soma e subtração – associatividade à esquerda
add_sub_expression :: ParsecT [Token] MemoryState IO Type
add_sub_expression = chainl1 mul_div_rem_expression add_sub_op

-- multiplicação, divisão e resto – associatividade à esquerda
mul_div_rem_expression :: ParsecT [Token] MemoryState IO Type
mul_div_rem_expression = chainl1 pow_expression mul_div_rem_op

-- potência – associatividade à direita
pow_expression :: ParsecT [Token] MemoryState IO Type
pow_expression = chainr1 unary_expression pow_op

-- unário (ex: -x)
unary_expression :: ParsecT [Token] MemoryState IO Type
unary_expression =
  try (do
    op <- unary_ops
    op <$> expression
  )
  <|> factor

-- fatores: unidades atômicas da expressão
factor :: ParsecT [Token] MemoryState IO Type
factor =
  -- fmap (:[]) nullToken
  try string_tokens
  <|> try function_call
  -- <|> try access_chain
  <|> do
    a <- idToken
    get_variable_value a <$> getState
  <|> try numeric_literal_tokens
  <|> try boolean_tokens
  <|>
  try (do
    a <- parenLeftToken
    b <- expression
    c <- parenRightToken
    return b
    -- return ([a] ++ b ++ [c])
  )

-- operadores binários
or_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
or_op = do
  Or (line, col) <- orToken
  return (\a b -> case (a, b) of
      (Boolean x, Boolean y) -> Boolean (x || y)
      (x, y) -> error $ binary_type_error "or" x y (line, col)
    )

and_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
and_op = do
  And (line, col) <- andToken;
  return (\a b -> case (a, b) of
      (Boolean x, Boolean y) -> Boolean (x && y)
      (x, y) -> error $ binary_type_error "and" x y (line, col)
    )

equal_different_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
equal_different_op =
  (do
    Equal (line, col) <- equalToken
    return (do_equal_op (line, col))
  )
  <|>
  (do
    NotEqual (line, col) <- notEqualToken
    return (do_different_op (line, col))
  )

do_equal_op :: (Int, Int) -> (Type -> Type -> Type)
do_equal_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y)   -> Boolean $ x == y
  (Floating x, Floating y) -> Boolean $ x == y
  (Integer x, Floating y)  -> Boolean $ fromIntegral x == y
  (Floating x, Integer y)  -> Boolean $ x == fromIntegral y
  (Str x, Str y)           -> Boolean $ x == y
  (Boolean x, Boolean y)   -> Boolean $ x == y
  (x, y) -> error $ binary_type_error "==" x y (line, col)

do_different_op :: (Int, Int) -> (Type -> Type -> Type)
do_different_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y)   -> Boolean $ x /= y
  (Floating x, Floating y) -> Boolean $ x /= y
  (Integer x, Floating y)  -> Boolean $ fromIntegral x /= y
  (Floating x, Integer y)  -> Boolean $ x /= fromIntegral y
  (Str x, Str y)           -> Boolean $ x /= y
  (Boolean x, Boolean y)   -> Boolean $ x /= y
  (x, y) -> error $ binary_type_error "!=" x y (line, col)

greater_lesser_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
greater_lesser_op =
  (do
    Greater (line, col) <- greaterToken
    return (do_greater_op (line, col))
  )
  <|>
  (do
    GreaterEq (line, col) <- greaterEqToken
    return (do_greater_eq_op (line, col))
  )
  <|>
  (do
    Less (line, col) <- lessToken
    return (do_less_op (line, col))
  )
  <|>
  (do
    LessEq (line, col) <- lessEqToken
    return (do_less_eq_op (line, col))
  )

do_greater_op :: (Int, Int) -> (Type -> Type -> Type)
do_greater_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y)   -> Boolean $ x > y
  (Floating x, Floating y) -> Boolean $ x > y
  (Integer x, Floating y)  -> Boolean $ fromIntegral x > y
  (Floating x, Integer y)  -> Boolean $ x > fromIntegral y
  (x, y) -> error $ binary_type_error ">" x y (line, col)

do_greater_eq_op :: (Int, Int) -> (Type -> Type -> Type)
do_greater_eq_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y)   -> Boolean $ x >= y
  (Floating x, Floating y) -> Boolean $ x >= y
  (Integer x, Floating y)  -> Boolean $ fromIntegral x >= y
  (Floating x, Integer y)  -> Boolean $ x >= fromIntegral y
  (x, y) -> error $ binary_type_error ">=" x y (line, col)

do_less_op :: (Int, Int) -> (Type -> Type -> Type)
do_less_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y)   -> Boolean $ x < y
  (Floating x, Floating y) -> Boolean $ x < y
  (Integer x, Floating y)  -> Boolean $ fromIntegral x < y
  (Floating x, Integer y)  -> Boolean $ x < fromIntegral y
  (x, y)-> error $ binary_type_error "<" x y (line, col)

do_less_eq_op :: (Int, Int) -> (Type -> Type -> Type)
do_less_eq_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y)   -> Boolean $ x <= y
  (Floating x, Floating y) -> Boolean $ x <= y
  (Integer x, Floating y)  -> Boolean $ fromIntegral x <= y
  (Floating x, Integer y)  -> Boolean $ x <= fromIntegral y
  (x, y) -> error $ binary_type_error "<=" x y (line, col)

add_sub_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
add_sub_op =
  (do
    Add (line, col) <- addToken
    return (do_add_op (line, col))
  )
  <|>
  (do
    Sub (line, col) <- subToken
    return (do_sub_op (line, col))
  )

do_add_op :: (Int, Int) -> (Type -> Type -> Type)
do_add_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x + y
  (Floating x, Integer y) -> Floating $ x + fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x + y
  (Floating x, Floating y) -> Floating $ x + y
  (Str x, Str y) -> Str $ x ++ y
  (Str x, y) -> Str $ x ++ type_to_string y
  (x, Str y) -> Str $ type_to_string x ++ y
  (x, y) -> error $ binary_type_error "+" x y (line, col)

do_number_add_op :: (Int, Int) -> (Type -> Type -> Type)
do_number_add_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x + y
  (Floating x, Integer y) -> Floating $ x + fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x + y
  (Floating x, Floating y) -> Floating $ x + y
  (x, y) -> error $ binary_type_error "+" x y (line, col)

do_sub_op :: (Int, Int) -> (Type -> Type -> Type)
do_sub_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x - y
  (Floating x, Integer y) -> Floating $ x - fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x - y
  (Floating x, Floating y) -> Floating $ x - y
  (x, y) -> error $ binary_type_error "-" x y (line, col)

mul_div_rem_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
mul_div_rem_op =
  (do
    Mul (line, col) <- mulToken;
    return (do_mul_op (line, col))
  )
  <|>
  (do
    Div (line, col) <- divToken;
    return (do_div_op (line, col))
  )
  <|>
  (do
    Rem (line, col) <- remToken;
    return (do_rem_op (line, col))
  )

do_mul_op :: (Int, Int) -> (Type -> Type -> Type)
do_mul_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x * y
  (Floating x, Integer y) -> Floating $ x * fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x * y
  (Floating x, Floating y) -> Floating $ x * y
  (x, y) -> error $ binary_type_error "*" x y (line, col)

do_div_op :: (Int, Int) -> (Type -> Type -> Type)
do_div_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x `div` y
  (Floating x, Integer y) -> Floating $ x / fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x / y
  (Floating x, Floating y) -> Floating $ x / y
  (x, y) -> error $ binary_type_error "/" x y (line, col)

do_rem_op :: (Int, Int) -> (Type -> Type -> Type)
do_rem_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x `mod` y
  (x, y) -> error $ binary_type_error "%" x y (line, col)

pow_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
pow_op = do
  Pow (line, col) <- powToken
  return (do_pow_op (line, col))

do_pow_op :: (Int, Int) -> (Type -> Type -> Type)
do_pow_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x ^ y
  (Floating x, Integer y) -> Floating $ x ** fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x ** y
  (Floating x, Floating y) -> Floating $ x ** y
  (x, y) -> error $ binary_type_error "^" x y (line, col)

-- operador unário
unary_ops :: ParsecT [Token] MemoryState IO (Type -> Type)
unary_ops =
  (do
    Sub (line, col) <- subToken;
    return (do_neg_op (line, col))
  )
    <|>
  (do
    Not (line, col) <- notToken;
    return (do_not_op (line, col))
  )

do_neg_op :: (Int, Int) -> (Type -> Type)
do_neg_op (line, col) = \t -> case t of
  Integer a  -> Integer (-a)
  Floating a -> Floating (-a)
  a          -> error $ unary_type_error "-" a (line, col)

do_not_op :: (Int, Int) -> (Type -> Type)
do_not_op (line, col) = \t -> case t of
  Boolean a -> Boolean (not a)
  a         -> error $ unary_type_error "not" a (line, col)

prioritary_comparison_ops :: ParsecT [Token] MemoryState IO Token
prioritary_comparison_ops =
  try greaterToken
  <|> try greaterEqToken
  <|> try lessToken
  <|> try lessEqToken

non_prioritary_comparison_ops :: ParsecT [Token] MemoryState IO Token
non_prioritary_comparison_ops =
  try equalToken
  <|> try notEqualToken

all_comparison_ops :: ParsecT [Token] MemoryState IO Token
all_comparison_ops =
  try non_prioritary_comparison_ops
  <|> try prioritary_comparison_ops

access_chain :: ParsecT [Token] MemoryState IO [Token]
access_chain = do
  a <- idToken
  b <- remaining_access_chain
  return (a : b)

remaining_access_chain :: ParsecT [Token] MemoryState IO [Token]
remaining_access_chain =
  try (do
    a <- dotToken
    b <- idToken
    rest <- remaining_access_chain
    return ([a] ++ [b] ++ rest)
  )
  -- <|>
  -- try (do
  --   a <- braceLeftToken
  --   b <- expression
  --   c <- braceRightToken
  --   rest <- remaining_access_chain
  --   return ([a] ++ b ++ [c] ++ rest)
  -- )
  -- <|>
  -- try (do
  --   a <- braceLeftToken
  --   b <- expression
  --   c <- commaToken
  --   d <- expression
  --   e <- braceRightToken
  --   rest <- remaining_access_chain
  --   return ([a] ++ b ++ [c] ++ d ++ [e] ++ rest)
  -- )
  <|> return []

function_call :: ParsecT [Token] MemoryState IO Type
function_call =
  -- try (do
  --   a <- idToken
  --   b <- parenLeftToken
  --   c <- expressions <|> return []
  --   d <- parenRightToken
  --   return c
  --   -- return ([a, b] ++ c ++ [d])
  -- )
  -- <|> 
  try scan_function

loop :: ParsecT [Token] MemoryState IO [Token]
loop = while <|> repeat_until <|> for

while :: ParsecT [Token] MemoryState IO [Token]
while = do
  a <- whileToken
  b <- parenLeftToken
  c <- expression
  d <- parenRightToken
  e <- bracketLeftToken
  f <- stmts
  g <- bracketRightToken
  return [a]
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g])

repeat_until :: ParsecT [Token] MemoryState IO [Token]
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
  return [a]
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ [f] ++ g ++ [h] ++ [j])

for :: ParsecT [Token] MemoryState IO [Token]
for = do
  a <- forToken
  b <- parenLeftToken
  c <- for_variable_initialization
  d <- for_condition
  e <- semiColonToken
  f <- for_assign
  g <- parenRightToken
  h <- bracketLeftToken
  i <- stmts
  j <- bracketRightToken
  return ([a] ++ [b] ++ c ++ d ++ [e] ++ f ++ [g] ++ [h]++ i ++ [j])

for_variable_initialization :: ParsecT [Token] MemoryState IO [Token]
for_variable_initialization =
  try (do
    a <- for_assign
    b <- semiColonToken
    return (a ++ [b])
  )
  <|> try variable_declaration_assignment

for_condition :: ParsecT [Token] MemoryState IO [Token]
for_condition =
  try (do
    a <- access_chain
    b <- all_comparison_ops
    c <- expression
    return a
    -- return (a ++ [b] ++ c)
  )
  <|>
  try (do
    a <- idToken
    b <- all_comparison_ops
    c <- expression
    return [a]
    -- return ([a] ++ [b] ++ c)
  )

for_assign :: ParsecT [Token] MemoryState IO [Token]
for_assign =
  try (do
    a <- access_chain
    b <- all_assign_tokens
    c <- expression
    return a
    -- return (a ++ [b] ++ c)
  )
  <|>
  try (do
    a <- idToken
    b <- all_assign_tokens
    c <- expression
    return [a]
    -- return ([a] ++ [b] ++ c)
  )

conditional :: ParsecT [Token] MemoryState IO [Token]
conditional = if_else <|> match_case

if_else :: ParsecT [Token] MemoryState IO [Token]
if_else = do
  (first, cond) <- cond_if
  next <- cond_else cond
  return (first ++ next)

cond_if :: ParsecT [Token] MemoryState IO ([Token], Bool)
cond_if = do
  exec <- executing <$> getState
  a <- ifToken
  b@(ParenLeft (line, column)) <- parenLeftToken
  cond <- expression
  d <- parenRightToken
  let result = check_condition "if" (line, column) cond
  condBool <- case result of
    Left err -> fail err
    Right cond -> do
        unless cond $ updateState (\st -> st { executing = False })
        return cond
  e <- bracketLeftToken
  f <- stmts
  g <- bracketRightToken
  updateState (\st -> st { executing = exec })
  return ([a], condBool)
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g])

check_condition :: String -> (Int, Int) -> Type  -> Either String Bool
check_condition _ _ (Boolean True) = Right True
check_condition _ _ (Boolean False) = Right False
check_condition conditional_name (line, column) t =
    Left $ "type error in \"" ++ conditional_name ++ "\" condition: expected boolean, got " ++
    show_pretty_type_values t ++ "; line: " ++ show line ++ ", column: " ++ show column

cond_else :: Bool -> ParsecT [Token] MemoryState IO [Token]
cond_else if_cond =
  (do
    exec <- executing <$> getState
    a <- elseToken
    b <- bracketLeftToken
    when if_cond $
        updateState (\st -> st { executing = False })
    c <- stmts
    d <- bracketRightToken
    updateState (\st -> st { executing = exec })
    return ([a] ++ [b] ++ c ++ [d])
  )
  <|>
  return []

match_case :: ParsecT [Token] MemoryState IO [Token]
match_case = do
  a <- matchToken
  b <- parenLeftToken
  c <- expression
  d <- parenRightToken
  e <- bracketLeftToken
  f <- cases
  h <- bracketRightToken
  return [a]
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [h])

cases :: ParsecT [Token] MemoryState IO [Token]
cases =
  try default_case
  <|>
  try (do
    a <- single_case
    b <- remaining_cases
    return (a ++ b)
  )

remaining_cases :: ParsecT [Token] MemoryState IO [Token]
remaining_cases =
  try default_case
  <|>
  try (do
    a <- single_case
    b <- remaining_cases
    return (a ++ b)
  )
  <|> return []

single_case :: ParsecT [Token] MemoryState IO [Token]
single_case = do
  a <- caseToken
  b <- case_expression
  c <- colonToken
  d <- stmts
  return ([a] ++ b ++ [c] ++ d)

case_expression :: ParsecT [Token] MemoryState IO [Token]
case_expression =
  -- try expression
  -- <|>
  try (do
    a <- parenLeftToken
    b <- expression
    c <- remaining_case_expression
    d <- parenRightToken
    return [a]
    -- return ([a] ++ b ++ c ++ [d])
  )

remaining_case_expression :: ParsecT [Token] MemoryState IO [Token]
remaining_case_expression =
  try (do
    a <- commaToken
    b <- expression
    c <- remaining_case_expression
    return [a]
    -- return ([a] ++ b ++ c)
  )
  <|> return []

default_case :: ParsecT [Token] MemoryState IO [Token]
default_case = do
    a <- defaultToken
    b <- colonToken
    c <- stmts
    return ([a] ++ [b] ++ c)

numeric_literal_tokens :: ParsecT [Token] MemoryState IO Type
numeric_literal_tokens = do
  a <- try intToken <|> try floatToken
  get_type_value a <$> getState

all_possible_type_tokens :: ParsecT [Token] MemoryState IO Type
all_possible_type_tokens =
  try (do
    a <- try idToken <|> try typeToken
    get_type_value a <$> getState
  )
  -- <|>
  -- try (do
  --   a <- matrixToken <|> vectorToken
  --   b <- lessToken
  --   c <- all_possible_type_tokens
  --   d <- greaterToken
  --   return ([a] ++ [b] ++ c ++ [d])
  -- )

string_tokens :: ParsecT [Token] MemoryState IO Type
string_tokens = do
  a <- stringToken
  get_type_value a <$> getState

boolean_tokens :: ParsecT [Token] MemoryState IO Type
boolean_tokens = do
  a <- boolToken
  get_type_value a <$> getState

print_procedure :: ParsecT [Token] MemoryState IO [Token]
print_procedure = do
  a <- printToken
  b <- parenLeftToken
  c <- expression <|> return (Str "")
  d <- parenRightToken
  e <- semiColonToken
  exec <- executing <$> getState
  when exec $ liftIO $ putStrLn $ type_to_string c
  return [a]

type_to_string :: Type -> String
type_to_string (Integer x) = show x
type_to_string (Floating x) = show x
type_to_string (Str x) = x
type_to_string (Boolean True) = "true"
type_to_string (Boolean False) = "false"
type_to_string p = show p

scan_function :: ParsecT [Token] MemoryState IO Type
scan_function = do
  a <- scanToken
  b <- parenLeftToken
  c <- parenRightToken
  exec <- executing <$> getState
  n <- if exec
        then liftIO (readLn :: IO Int)
        else return 0
  return $ Integer n

all_escapes_stmts :: ParsecT [Token] MemoryState IO [Token]
all_escapes_stmts = do
  a <- continueToken <|> leaveToken <|> breakToken
  b <- semiColonToken
  return (a : [b])

-- funções para a tabela de símbolos

get_type_value :: Token -> MemoryState -> Type
get_type_value (Type "int" _) _ = Integer 0
get_type_value (Type "string" _) _ = Str ""
get_type_value (Type "float" _) _ = Floating 0.0
get_type_value (Type "bool" _) _ = Boolean False
get_type_value (Int x _) _ = Integer x
get_type_value (String x _) _ = Str x
get_type_value (Float x _) _ = Floating x
get_type_value (Bool x _) _ = Boolean x
get_type_value token@(Id x _) (MemoryState _ table _) = lookup_type token table

get_variable_value :: Token -> MemoryState -> Type
get_variable_value token@(Id x _) (MemoryState table _ _) = lookup_variable token table

lookup_variable :: Token -> [(String, Type)] -> Type
lookup_variable (Id name (line, column)) [] = error $ "undefined variable \"" ++ name ++ "\": line " ++ show line ++ " column " ++ show column
lookup_variable token@(Id name _) ((name2, typ) : rest)
    | name == name2 = typ
    | otherwise = lookup_variable token rest

lookup_type :: Token -> [Type] -> Type
lookup_type (Id name (line, column)) [] = error $ "undefined type \"" ++ name ++ "\": line " ++ show line ++ " column " ++ show column
lookup_type token@(Id name _) (t@(Record name2 _) : rest)
    | name == name2 = t
    | otherwise = lookup_type token rest

typetable_insert :: Type -> MemoryState -> MemoryState
typetable_insert t@(Record current_name fields) st@(MemoryState _ table _) = do
    st {typetable = table ++ [t]}

symtable_insert :: (Token, Type) -> MemoryState -> MemoryState
symtable_insert (Id name (line, column), typ) st@(MemoryState current_table _ _)= do
    let declared_names = [name | (name, _) <- current_table] in
        if name `elem` declared_names
        then error $ "variable \"" ++ name ++ "\" already declared: line " ++ show line ++ " column " ++ show column
        else st { symtable = current_table ++ [(name, typ)] }

symtable_update :: (Token, Type) -> MemoryState -> MemoryState
symtable_update (Id id (line, column), _) (MemoryState [] _ _) = error $ "variable \"" ++ id ++ "\" not found: line " ++ show line ++ " column " ++ show column
symtable_update (Id id1 p1, v1) st@(MemoryState ((id2, v2) : t) _ _) =
  if id1 == id2
    then st {symtable = (id1, v1) : t}
    else st {symtable = (id2, v2) : symtable (symtable_update (Id id1 p1, v1) st {symtable = t})}

-- symtable_remove :: (Token, Token) -> MemoryState -> MemoryState
-- symtable_remove _ [] = fail "variable not found"
-- symtable_remove (id1, v1) ((id2, v2) : t) =
--   if id1 == id2
--     then t
--     else (id2, v2) : symtable_remove (id1, v1) t

compare_type_base :: Type -> Type -> Bool
compare_type_base first_type second_type = case (first_type, second_type) of
  (Integer _, Integer _)     -> True
  (Floating _, Floating _)   -> True
  (Str _, Str _)             -> True
  (Boolean _, Boolean _)     -> True
  (Record first_record _, Record second_record _) -> first_record == second_record
  _                          -> False

extract_base_type :: Type -> Type
extract_base_type t = case t of
  Integer _ -> Integer 0
  Floating _ -> Floating 0.0
  Str _ -> Str ""
  Boolean _ -> Boolean False
  Record name _ -> Record name []

--- funções para auxiliar na impressão de mensagens de erro

show_pretty_types :: Type -> String
show_pretty_types t = case t of
  Integer _ -> "int"
  Floating _ -> "float"
  Str _ -> "string"
  Boolean _ -> "bool"
  Record name _ -> show name

show_pretty_type_values :: Type -> String
show_pretty_type_values t = case t of
  Integer x -> "int (" ++ show x ++ ")"
  Floating x -> "float (" ++ show x ++ ")"
  Str x -> "string (" ++ show x ++ ")"
  Boolean x -> "bool (" ++ map toLower (show x) ++ ")"
  Record name x -> show name ++ show x ++ ")"

binary_type_error :: String -> Type -> Type -> (Int, Int) -> a
binary_type_error op_name first_type second_type (line, column) =
  error $ "type error in \"" ++ op_name ++ "\": unexpected parameter types " ++
  show_pretty_type_values first_type ++ " and " ++ show_pretty_type_values second_type ++
  "; line: " ++ show line ++ ", column: " ++ show column

unary_type_error :: String -> Type -> (Int, Int) -> a
unary_type_error op_name t (line, column) =
  error $ "type error in \"" ++ op_name ++ "\": unexpected parameter type " ++
  show_pretty_type_values t ++ "; line: " ++ show line ++ ", column: " ++ show column

condition_type_error :: String -> Type -> (Int, Int) -> a
condition_type_error conditional_name t (line, column) =
  error $ "type error in \"" ++ conditional_name ++ "\" condition: expected boolean, got " ++
  show_pretty_type_values t ++ "; line: " ++ show line ++ ", column: " ++ show column

print_symtable :: ParsecT [Token] MemoryState IO ()
print_symtable = do
  st <- getState
  liftIO $ putStrLn ("symtable: " ++ show (symtable st) ++ "\n")

print_types :: ParsecT [Token] MemoryState IO ()
print_types = do
  st <- getState
  liftIO $ putStrLn ("user defined types: " ++ show (typetable st) ++ "\n")

-- invocação do parser para o símbolo de partida

parser :: [Token] -> IO (Either ParseError ())
parser = runParserT program (MemoryState { symtable = [] , typetable = [], executing = False}) "Parsing error!"

main :: IO ()
main = do
  tokens <- getTokens "programa.pe"
  result <- parser tokens
  case result of
    Left err -> print err
    Right ans -> return ()
