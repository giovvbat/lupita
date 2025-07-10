{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
module Main (main) where

import Control.Arrow (Arrow (first))
import Control.Monad (Functor (fmap), Monad (return), when, unless)
import Control.Monad.IO.Class
import GHC.Float (divideDouble)
import GHC.RTS.Flags (TraceFlags (user))
import Lexer
import Text.Parsec
import Data.Char (toLower)
import Data.List (nub, (\\))
import Control.Monad.Trans.Class (lift)

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
-- nome, parametros, tipo de retorno e código
-- data SubP = Proc ... | Func ...
--
-- flag se está executando
-- falsa se está em declaração de subprograma
-- verdadeira quando entra na main

-- nome da variável/constante, tipo e valor, flag para identificar se é variável (true) ou não (constante)
type SymbolTable = [(String, Type, Bool)]

data Type = Integer Int | Floating Float | Str String | Boolean Bool | Record String [(String, Type, Bool)] | Enumeration String [(String, Type, Bool)]
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
    initial_declarations
  )
  <|>
  try (do
    a <- variable_declarations
    initial_declarations
  )
  <|> try (do
    a <- const_declarations
    initial_declarations
  )
  <|> try (do
    a <- data_structures_declarations
    initial_declarations
  )
  <|> try (do
    a <- subprograms
    initial_declarations
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
    a@(Struct (line, col)) <- structToken
    b <- idToken
    c <- bracketLeftToken
    d <- user_defined_types_declarations
    e <- bracketRightToken
    let Id b_name _ = b in do
        updateState (typetable_insert (Record b_name d) (line, col))
        return ([a] ++ [b] ++ [c] ++ [e])

enum :: ParsecT [Token] MemoryState IO [Token]
enum = do
    a@(Enum (line, col)) <- enumToken
    b <- idToken
    c <- bracketLeftToken
    d <- user_defined_types_declarations
    e <- bracketRightToken
    let Id b_name _ = b in do
        updateState (typetable_insert (Enumeration b_name d) (line, col))
        return ([a] ++ [b] ++ [c] ++ [e])

user_defined_types_declarations :: ParsecT [Token] MemoryState IO [(String, Type, Bool)]
user_defined_types_declarations = do
  first <- try variable_declarations
  next <- remaining_user_defined_types_declarations
  return (first : next)

remaining_user_defined_types_declarations :: ParsecT [Token] MemoryState IO [(String, Type, Bool)]
remaining_user_defined_types_declarations =
  (do
    first <- try variable_declarations
    rest <- remaining_user_defined_types_declarations
    return (first : rest)
  )
  <|> return []

variable_declarations :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_declarations =
  try variable_declaration
  <|> try variable_declaration_assignment
  <|> try variable_guess_declaration_assignment

variable_declaration :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_declaration = do
  a <- idToken
  b <- all_possible_type_tokens
  c <- semiColonToken
  s <- getState
  when (executing s) $
    updateState (symtable_insert (a, b, True))
  let Id a_name _ = a in
    return (a_name, b, True)
  -- return ([a] ++ b ++ [c])

variable_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_declaration_assignment = do
  a@(Id name (line, col)) <- idToken
  b <- all_possible_type_tokens
  c <- assignToken
  d <- expression
  e <- semiColonToken
  s <- getState
  let expr_base_type = extract_base_type d
  let declared_base_type = extract_base_type b
  if declared_base_type == expr_base_type
    then do
      when (executing s) $
        updateState (symtable_insert (a, d, True))
      return (name, d, True)
    else
      fail $ variable_type_error_msg name b d (line, col)

variable_guess_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_guess_declaration_assignment = do
  a@(Id name _) <- idToken
  b <- guessToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  s <- getState
  when (executing s) $
    updateState (symtable_insert (a, d, True))
  let Id name _ = a
    in return (name, d, True)

const_declarations :: ParsecT [Token] MemoryState IO (String, Type, Bool)
const_declarations =
  try const_declaration_assignment
  <|> try const_guess_declaration_assignment

const_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type, Bool)
const_declaration_assignment = do
  a@(Id name (line, col)) <- idToken
  b <- constToken
  c <- all_possible_type_tokens
  d <- assignToken
  e <- expression
  f <- semiColonToken
  s <- getState
  let expr_base_type = extract_base_type e
  let declared_base_type = extract_base_type c
  if declared_base_type == expr_base_type
    then do
      if is_main_four_type declared_base_type
        then do
        when (executing s) $
          updateState (symtable_insert (a, e, False))
        return (name, e, False)
        else
          fail $ const_guess_declaration_assignment_error_msg (line, col)
    else
      fail $ variable_type_error_msg name declared_base_type expr_base_type (line, col)

const_guess_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type, Bool)
const_guess_declaration_assignment = do
  a@(Id name (line, col)) <- idToken
  b <- constToken
  c <- guessToken
  d <- assignToken
  e <- expression
  f <- semiColonToken
  s <- getState
  if is_main_four_type e
    then do
    when (executing s) $
      updateState (symtable_insert (a, e, False))
    let Id name _ = a
      in return (name, e, False)
    else
      fail $ const_guess_declaration_assignment_error_msg (line, col)

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
  <|>
  try (do
    procedure_call
    return []
  )
  <|>
  try (do
    variable_declarations
    return []
  )
  <|>
  try (do
    const_declarations
    return []
  )
  <|> try data_structures_declarations
  <|> try function_return
  --- obs.: manter assign por último! caso contrário, podemos ter conflito com o access_chain nas declarações de constantes/variáveis
  <|> try assign

all_assign_tokens :: ParsecT [Token] MemoryState IO ((Int, Int) -> Type -> Type -> Type)
all_assign_tokens =
  try (do
    a <- assignToken
    return assign_eq
  )
  <|>
  try (do
    AddAssign pos@(line, col) <- addAssignToken
    return do_number_add_op
  )
  <|>
  try (do
    SubAssign pos@(line, col) <- subAssignToken
    return do_sub_op
  )
  <|>
  try (do
    MulAssign pos@(line, col) <- mulAssignToken
    return do_mul_op
  )
  <|>
  try (do
    DivAssign pos@(line, col) <- divAssignToken
    return do_div_op
  )
  <|>
  try (do
    RemAssign pos@(line, col) <- remAssignToken
    return do_rem_op
  )
  <|>
  try (do
    PowAssign pos@(line, col) <- powAssignToken
    return do_pow_op
  )

assign_eq :: (Int, Int) -> Type -> Type -> Type
assign_eq _ assignee assigned = assigned -- retorna o valor original

assign :: ParsecT [Token] MemoryState IO [Token]
assign = do
  (access_type, token_chain) <- access_chain
  assign_function <- all_assign_tokens
  b <- expression
  c <- semiColonToken
  s <- getState
  let access_base_type = extract_base_type access_type
  let expr_base_type = extract_base_type b
  let (Id _ (line, col)) = head token_chain
  let new_value = assign_function (line, col) access_type b
  if access_base_type == expr_base_type
    then do
      when (executing s) $
        updateState (symtable_update_by_access_chain token_chain new_value)
      return token_chain
    else
      fail $ assign_type_error_msg access_type b (line, col)

procedure_call :: ParsecT [Token] MemoryState IO ()
procedure_call =
  try (do
    a <- idToken
    b <- parenLeftToken
    c <- expressions <|> return []
    d <- parenRightToken
    e <- semiColonToken
    return ()
  )
  <|>
  try (do
    a <- print_procedure
    return ()
  )

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
  try string_tokens
  <|> try function_call
  <|>
  try (do
    a@(t, _) <- access_chain
    return t
  )
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

access_chain :: ParsecT [Token] MemoryState IO (Type, [Token])
access_chain = do
  first_id@(Id name (line, col)) <- idToken
  st <- getState
  let current_type = lookup_variable first_id (symtable st)
  (final_type, chain_tokens) <- access_chain_tail current_type (line, col)
  return (final_type, first_id : chain_tokens)

access_chain_tail :: Type -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, [Token])
access_chain_tail current_type pos =
  try (do
    (next_type, tokens) <- field_access current_type pos
    (final_type, rest_tokens) <- access_chain_tail next_type pos
    return (final_type, tokens ++ rest_tokens)
  ) <|> return (current_type, [])

field_access :: Type -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, [Token])
field_access (Record record_name fields) pos = access_from_fields record_name fields pos
field_access (Enumeration enum_name fields) pos = access_from_fields enum_name fields pos
field_access t (line, col) = fail $ "type " ++ show_pretty_type_values t ++ " does not support field access"
  ++ "; line: " ++ show line ++ ", column: " ++ show col

access_from_fields :: String -> [(String, Type, Bool)] -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, [Token])
access_from_fields type_name entries _ = do
  dot <- dotToken
  field@(Id field_name (line, col)) <- idToken
  let fields_map = map (\(n, t, _) -> (n, t)) entries
  case lookup field_name fields_map of
    Just t -> return (t, [field])
    Nothing -> fail $
      "cannot access field \"" ++ field_name ++ "\" on value of type " ++ type_name
      ++ "; line: " ++ show line ++ ", column: " ++ show col

-- remaining_access_chain :: ParsecT [Token] MemoryState IO [Token]
-- remaining_access_chain =
--   try (do
--     a <- dotToken
--     b <- idToken
--     rest <- remaining_access_chain
--     return ([a] ++ [b] ++ rest)
--   )
--   -- <|>
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
  -- <|> return []

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
  state <- getState
  let exec = executing state

  a <- whileToken
  b@(ParenLeft (line, column)) <- parenLeftToken
  inputBeforeCond <- getInput
  cond <- expression
  inputAfterCond <- getInput
  d <- parenRightToken

  let condTokens = take (length inputBeforeCond - length inputAfterCond) inputBeforeCond

  e <- bracketLeftToken
  inputBefore <- getInput
  updateState (\st -> st { executing = False })
  f <- stmts
  updateState (\st -> st { executing = exec })
  inputAfter <- getInput
  g <- bracketRightToken

  let bodyTokens = take (length inputBefore - length inputAfter) inputBefore

  when exec $ do
    run_loop False condTokens bodyTokens (line, column)

  return [a]
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g])

repeat_until :: ParsecT [Token] MemoryState IO [Token]
repeat_until = do
    exec <- executing <$> getState

    a <- repeatToken
    b <- bracketLeftToken

    inputBefore <- getInput
    c <- stmts
    inputAfter <- getInput

    d <- bracketRightToken

    e <- untilToken
    ParenLeft (line, column) <- parenLeftToken

    inputBeforeCond <- getInput
    cond <- expression
    inputAfterCond <- getInput

    h <- parenRightToken
    j <- semiColonToken

    let bodyTokens = take (length inputBefore - length inputAfter) inputBefore
    let condTokens = take (length inputBeforeCond - length inputAfterCond) inputBeforeCond

    when exec $ do
        run_loop True condTokens bodyTokens (line, column)

    return [a]

run_loop :: Bool -> [Token] -> [Token] -> (Int, Int) -> ParsecT [Token] MemoryState IO ()
run_loop negateCondition condTokens bodyTokens (line, column) = do
    state <- getState

    condResult <- lift $ runParserT expression state "" condTokens
    condBool <- case condResult of
        Left parseErr -> fail (show parseErr)
        Right condExpr -> case check_condition "while" (line, column) condExpr of
            Left err -> fail err
            Right boolVal -> return (if negateCondition then not boolVal else boolVal)

    when condBool $ do
        state' <- getState
        result <- lift $ runParserTWithState stmts state' bodyTokens
        case result of
            Left err -> fail (show err)
            Right (_, newState) -> do
                putState newState
                run_loop negateCondition condTokens bodyTokens (line, column)

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
  -- <|> try variable_declaration_assignment

for_condition :: ParsecT [Token] MemoryState IO [Token]
for_condition =
  try (do
    a <- idToken
    b <- all_comparison_ops
    c <- expression
    return [a]
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
    a <- idToken
    b <- all_assign_tokens
    c <- expression
    return [a]
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
  exec <- executing <$> getState
  a <- matchToken
  b <- parenLeftToken
  expr <- expression
  d <- parenRightToken
  e <- bracketLeftToken
  f <- cases expr
  h <- bracketRightToken
  return [a]
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [h])

cases :: Type -> ParsecT [Token] MemoryState IO Bool
cases expr =
  try $ default_case False
  <|>
  try (do
    matched_first <- single_case expr False
    remaining_cases expr matched_first
  )

remaining_cases :: Type -> Bool -> ParsecT [Token] MemoryState IO Bool
remaining_cases expr matched =
  try $ default_case matched
  <|>
  try (do
    matched_current <- single_case expr matched
    remaining_cases expr matched_current
  )
  <|> return matched

single_case :: Type -> Bool -> ParsecT [Token] MemoryState IO Bool
single_case expr matched = do
  exec <- executing <$> getState
  a <- caseToken
  cond <- case_expression expr
  c <- colonToken
  when (not cond || matched) $
    updateState (\st -> st { executing = False });
  d <- stmts
  updateState (\st -> st { executing = exec })
  return (cond || matched)

case_expression :: Type -> ParsecT [Token] MemoryState IO Bool
case_expression expr =
  try (do
      e <- expression
      return $ e == expr
  )
  <|>
  try (do
    a <- parenLeftToken
    e <- expression
    rest <- remaining_case_expression expr
    d <- parenRightToken
    return $ (e == expr) || rest
  )

remaining_case_expression :: Type -> ParsecT [Token] MemoryState IO Bool
remaining_case_expression expr =
  try (do
    a <- commaToken
    e <- expression
    rest <- remaining_case_expression expr
    return $ (e == expr) || rest
  )
  <|> return False

default_case :: Bool -> ParsecT [Token] MemoryState IO Bool
default_case matched = do
    exec <- executing <$> getState
    a <- defaultToken
    b <- colonToken
    when matched $
        updateState (\st -> st { executing = False })
    c <- stmts
    updateState (\st -> st { executing = exec })
    return True

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

print_procedure :: ParsecT [Token] MemoryState IO ()
print_procedure = do
  a <- printToken
  b <- parenLeftToken
  c <- expression <|> return (Str "")
  d <- parenRightToken
  e <- semiColonToken
  exec <- executing <$> getState
  when exec $ liftIO $ putStrLn $ type_to_string c

type_to_string :: Type -> String
type_to_string (Integer x) = show x
type_to_string (Floating x) = show x
type_to_string (Str x) = x
type_to_string (Boolean True) = "true"
type_to_string (Boolean False) = "false"
type_to_string (Record name fields) = show_fields fields
type_to_string (Enumeration name fields) = show_fields fields

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

lookup_variable :: Token -> SymbolTable -> Type
lookup_variable (Id name (line, column)) [] =
  error $ "undefined variable \"" ++ name ++ "\" in scope; line " ++ show line ++ " column " ++ show column
lookup_variable token@(Id name _) ((name2, typ, _) : rest)
  | name == name2 = typ
  | otherwise     = lookup_variable token rest

lookup_type :: Token -> [Type] -> Type
lookup_type (Id name (line, column)) [] =
  error $ "undefined type \"" ++ name ++ "\"; line " ++ show line ++ " column " ++ show column
lookup_type token@(Id name _) (t : rest) = case t of
  Record name2 _ | name == name2 -> t
  Enumeration  name2 _ | name == name2 -> t
  _ -> lookup_type token rest

typetable_insert :: Type -> (Int, Int) -> MemoryState -> MemoryState
typetable_insert t@(Record name fields) (line, column) st@(MemoryState _ table _) =
  let
    field_names = map (\(fname, _, _) -> fname) fields
    duplicates = field_names \\ nub field_names
    existing_type_names = map get_type_name table
  in
    if name `elem` existing_type_names
      then error $ "type \"" ++ name ++ "\" already defined, alternative name must be provided; line " ++ show line ++ " column " ++ show column
    else if not (null duplicates)
      then error $ "duplicate field names in record " ++ name ++ ": " ++ show duplicates ++ "; line " ++ show line ++ " column " ++ show column
    else st {typetable = table ++ [t]}
typetable_insert t@(Enumeration name fields) (line, column) st@(MemoryState _ table _) =
  let
    label_names = map (\(label, _, _) -> label) fields
    duplicates = label_names \\ nub label_names
    existing_type_names = map get_type_name table
  in
    if name `elem` existing_type_names
      then error $ "type \"" ++ name ++ "\" already defined, alternative name must be provided; line " ++ show line ++ " column " ++ show column
    else if not (null duplicates)
      then error $ "duplicate labels in enumeration " ++ name ++ ": " ++ show duplicates ++ "; line " ++ show line ++ " column " ++ show column
    else st {typetable = table ++ [t]}
typetable_insert _ _ st = st  -- ignora outros tipos

symtable_insert :: (Token, Type, Bool) -> MemoryState -> MemoryState
symtable_insert (Id name (line, column), typ, is_variable) st@(MemoryState current_table _ _) = do
  let declared_names = [name | (name, _, _) <- current_table] in
    if name `elem` declared_names
      then error $ "variable \"" ++ name ++ "\" already declared in scope; line " ++ show line ++ " column " ++ show column
      else st { symtable = current_table ++ [(name, typ, is_variable)] }

-- atualiza o valor na tabela de símbolos usando uma cadeia de acesso
symtable_update_by_access_chain :: [Token] -> Type -> MemoryState -> MemoryState
symtable_update_by_access_chain [] _ st = st -- Cadeia vazia, nada a fazer
symtable_update_by_access_chain [Id name pos] new_val st = symtable_update (Id name pos, new_val) st
symtable_update_by_access_chain (Id base_name pos : rest) new_val st@(MemoryState sym_table type_table executing) =
  case lookup base_name [(n, v) | (n, v, _) <- sym_table] of
    Nothing -> error $ "undefined variable \"" ++ base_name ++ "\"; line " ++ show (fst pos) ++ " column " ++ show (snd pos)
    Just base_type ->
      let updated_type = update_nested_type base_type rest new_val pos
          updated_sym_table = update_entry base_name updated_type sym_table
      in st { symtable = updated_sym_table }
symtable_update_by_access_chain _ _ st = st -- fallback seguro

update_nested_type :: Type -> [Token] -> Type -> (Int, Int) -> Type
update_nested_type _ [] _ _ = error "invalid access chain: empty tail"
update_nested_type (Record name fields) (Id field_name _ : rest) new_val pos =
  let updated_fields = map update_field fields
      update_field (fname, ftype, is_var)
        | fname == field_name =
            if null rest
              then (fname, new_val, is_var)
              else (fname, update_nested_type ftype rest new_val pos, is_var)
        | otherwise = (fname, ftype, is_var)
  in Record name updated_fields
update_nested_type (Enumeration name fields) (Id label_name _ : rest) new_val pos =
  let updated_labels = map update_label fields
      update_label (lname, ltype, is_var)
        | lname == label_name =
            if null rest
              then (lname, new_val, is_var)
              else (lname, update_nested_type ltype rest new_val pos, is_var)
        | otherwise = (lname, ltype, is_var)
  in Enumeration name updated_labels
update_nested_type t (_ : _) _ (line, col) =
  error $ "cannot assign to field in non-structured type: " ++ show_pretty_type_values t ++
          "; line " ++ show line ++ ", column " ++ show col

update_entry :: String -> Type -> SymbolTable -> SymbolTable
update_entry _ _ [] = []
update_entry name new_val ((n, t, is_var) : rest)
  | n == name =
      if not is_var
        then error $ "cannot assign to constant \"" ++ name ++ "\""
        else (n, new_val, is_var) : rest
  | otherwise = (n, t, is_var) : update_entry name new_val rest


symtable_update :: (Token, Type) -> MemoryState -> MemoryState
symtable_update (Id id_name (line, column), new_value) (MemoryState [] _ _) =
  error $ "variable \"" ++ id_name ++ "\" not found in scope; line " ++ show line ++ " column " ++ show column
symtable_update (Id id_name pos, new_value) st@(MemoryState ((name, old_value, is_variable) : rest) type_table executing) =
  if id_name == name
    then
      if not is_variable
        then error $ "cannot assign to constant \"" ++ id_name ++ "\"; line " ++ show (fst pos) ++ " column " ++ show (snd pos)
        else st { symtable = (id_name, new_value, is_variable) : rest }
    else
      let MemoryState updated_symtable updated_type_table updated_executing =
            symtable_update (Id id_name pos, new_value) (MemoryState rest type_table executing)
      in MemoryState ((name, old_value, is_variable) : updated_symtable) updated_type_table updated_executing

-- symtable_remove :: (Token, Token) -> MemoryState -> MemoryState
-- symtable_remove _ [] = fail "variable not found"
-- symtable_remove (id1, v1) ((id2, v2) : t) =
--   if id1 == id2
--     then t
--     else (id2, v2) : symtable_remove (id1, v1) t

is_main_four_type :: Type -> Bool
is_main_four_type t = case t of
  Integer _  -> True
  Floating _ -> True
  Str _      -> True
  Boolean _  -> True
  _          -> False

compare_type_base :: Type -> Type -> Bool
compare_type_base first_type second_type = case (first_type, second_type) of
  (Integer _, Integer _)     -> True
  (Floating _, Floating _)   -> True
  (Str _, Str _)             -> True
  (Boolean _, Boolean _)     -> True
  (Record first_record _, Record second_record _) -> first_record == second_record
  (Enumeration first_enumeration _, Enumeration second_enumeration _) -> first_enumeration == second_enumeration
  _                          -> False

extract_base_type :: Type -> Type
extract_base_type t = case t of
  Integer _ -> Integer 0
  Floating _ -> Floating 0.0
  Str _ -> Str ""
  Boolean _ -> Boolean False
  Record name _ -> Record name []
  Enumeration name _ -> Record name []

get_type_name :: Type -> String
get_type_name (Record name _) = name
get_type_name (Enumeration name _) = name
get_type_name _ = ""

--- funções para auxiliar na impressão de mensagens de erro

show_pretty_types :: Type -> String
show_pretty_types t = case t of
  Integer _ -> "int"
  Floating _ -> "float"
  Str _ -> "string"
  Boolean _ -> "bool"
  Record name _ -> name
  Enumeration name _ -> name

show_pretty_type_values :: Type -> String
show_pretty_type_values t = case t of
  Integer x -> "int (" ++ show x ++ ")"
  Floating x -> "float (" ++ show x ++ ")"
  Str x -> "string (" ++ show x ++ ")"
  Boolean x -> "bool (" ++ map toLower (show x) ++ ")"
  Record name fields -> name ++ " (" ++ show_fields fields ++ ")"
  Enumeration name values -> name ++ " (" ++ show_fields values ++ ")"

-- Função auxiliar para imprimir os campos/labels formatados
show_fields :: [(String, Type, Bool)] -> String
show_fields [] = ""
show_fields [(fname, ftype, _)] = fname ++ ": " ++ show_pretty_type_values ftype
show_fields ((fname, ftype, _) : rest) = fname ++ ": " ++ show_pretty_type_values ftype ++ ", " ++ show_fields rest

binary_type_error :: String -> Type -> Type -> (Int, Int) -> a
binary_type_error op_name first_type second_type (line, column) =
  error $ "type error in \"" ++ op_name ++ "\": unexpected parameter types " ++
  show_pretty_type_values first_type ++ " and " ++ show_pretty_type_values second_type ++
  "; line: " ++ show line ++ ", column: " ++ show column

variable_type_error_msg :: String -> Type -> Type -> (Int, Int) -> String
variable_type_error_msg variable_name expected_type provided_type (line, column) = "type error: variable " ++ show variable_name
  ++ " expects type " ++ show_pretty_types expected_type ++
  ", but got " ++ show_pretty_type_values provided_type ++
  "; line: " ++ show line ++ ", column: " ++ show column

unary_type_error :: String -> Type -> (Int, Int) -> a
unary_type_error op_name t (line, column) =
  error $ "type error in \"" ++ op_name ++ "\": unexpected parameter type " ++
  show_pretty_type_values t ++ "; line: " ++ show line ++ ", column: " ++ show column

assign_type_error_msg :: Type -> Type -> (Int, Int) -> String
assign_type_error_msg expected_type provided_type (line, column) = "type error in assign operation: variable expects type " ++
  show_pretty_types expected_type ++ ", but got " ++ show_pretty_type_values provided_type ++ " instead; line: " ++ show line ++ ", column: " ++ show column

condition_type_error :: String -> Type -> (Int, Int) -> a
condition_type_error conditional_name t (line, column) =
  error $ "type error in \"" ++ conditional_name ++ "\" condition: expected boolean, got " ++
  show_pretty_type_values t ++ "; line: " ++ show line ++ ", column: " ++ show column

const_guess_declaration_assignment_error_msg :: (Int, Int) -> String
const_guess_declaration_assignment_error_msg (line, column) = "constants are of no bindability to user-defined types; line: " ++ show line ++ ", column: " ++ show column

print_symtable :: ParsecT [Token] MemoryState IO ()
print_symtable = do
  st <- getState
  liftIO $ putStrLn ("\nsymtable: " ++ show (symtable st))

print_types :: ParsecT [Token] MemoryState IO ()
print_types = do
  st <- getState
  liftIO $ putStrLn ("user defined types: " ++ show (typetable st))

runParserTWithState
    :: ParsecT [Token] MemoryState IO a
    -> MemoryState
    -> [Token]
    -> IO (Either ParseError (a, MemoryState))
runParserTWithState parser initialState tokens = do
    runParserT (do
        a <- parser
        s <- getState
        return (a, s)
        ) initialState "" tokens

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
