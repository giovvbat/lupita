{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main) where

import Control.Arrow (Arrow (first))
import Control.Monad (Functor (fmap), Monad (return), when)
import Control.Monad.IO.Class
import GHC.Float (divideDouble)
import GHC.RTS.Flags (TraceFlags (user))
import Lexer
import Text.Parsec

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

data Type = Inteiro Int | Flutuante Float | Str String | Booleano Bool | Registro String [(String, Type)]
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
  print_symtable
  print_types
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
        updateState (typetable_insert $ Registro b_name d)
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
  <|> try function_return

all_assign_tokens :: ParsecT [Token] MemoryState IO Token
all_assign_tokens =
  try addAssignToken
  <|> try subAssignToken
  <|> try mulAssignToken
  <|> try divAssignToken
  <|> try remAssignToken
  <|> try powAssignToken
  <|> try assignToken

assign :: ParsecT [Token] MemoryState IO [Token]
assign =
  try (do
    -- a <- access_chain
    a <- idToken
    b <- all_assign_tokens
    c <- expression
    d <- semiColonToken
    s <- getState
    when (executing s) $
        updateState (symtable_update (a, c))
    return [a]
    -- return ([a] ++ [b] ++ [c] ++ [d])
  )
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
    get_variable a <$> getState
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
  op <- orToken;
  return (\a b -> case (a, b) of
      (Booleano x, Booleano y) -> Booleano (x || y)
      (_, _) -> error "tipo inesperado"
    )

and_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
and_op = do
  op <- andToken;
  return (\a b -> case (a, b) of
      (Booleano x, Booleano y) -> Booleano (x && y)
      (_, _) -> error "tipo inesperado"
    )

equal_different_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
equal_different_op = (do
        op <- equalToken
        return do_equal_op
    ) <|>
    (do
        op <- notEqualToken
        return do_different_op
    )

do_equal_op :: Type -> Type -> Type
do_equal_op (Inteiro a) (Inteiro b) = Booleano $ a == b
do_equal_op (Flutuante a) (Flutuante b) = Booleano $ a == b
do_equal_op (Inteiro a) (Flutuante b) = Booleano $ fromIntegral a == b
do_equal_op (Flutuante a) (Inteiro b) = Booleano $ a == fromIntegral b
do_equal_op (Str a) (Str b) = Booleano $ a == b
do_equal_op (Booleano a) (Booleano b) = Booleano $ a == b

do_different_op :: Type -> Type -> Type
do_different_op (Inteiro a) (Inteiro b) = Booleano $ a /= b
do_different_op (Flutuante a) (Flutuante b) = Booleano $ a /= b
do_different_op (Inteiro a) (Flutuante b) = Booleano $ fromIntegral a /= b
do_different_op (Flutuante a) (Inteiro b) = Booleano $ a /= fromIntegral b
do_different_op (Str a) (Str b) = Booleano $ a /= b
do_different_op (Booleano a) (Booleano b) = Booleano $ a /= b

greater_lesser_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
greater_lesser_op = (do
        op <- greaterToken
        return do_greater_op
    ) <|>
    (do
        op <- greaterEqToken
        return do_greater_eq_op
    ) <|>
    (do
        op <- lessToken
        return do_less_op
    ) <|>
    (do
        op <- lessEqToken
        return do_less_eq_op
    )

do_greater_op :: Type -> Type -> Type
do_greater_op (Inteiro a) (Inteiro b) = Booleano $ a > b
do_greater_op (Flutuante a) (Flutuante b) = Booleano $ a > b
do_greater_op (Inteiro a) (Flutuante b) = Booleano $ fromIntegral a > b
do_greater_op (Flutuante a) (Inteiro b) = Booleano $ a > fromIntegral b

do_greater_eq_op :: Type -> Type -> Type
do_greater_eq_op (Inteiro a) (Inteiro b) = Booleano $ a >= b
do_greater_eq_op (Flutuante a) (Flutuante b) = Booleano $ a >= b
do_greater_eq_op (Inteiro a) (Flutuante b) = Booleano $ fromIntegral a >= b
do_greater_eq_op (Flutuante a) (Inteiro b) = Booleano $ a >= fromIntegral b

do_less_op :: Type -> Type -> Type
do_less_op (Inteiro a) (Inteiro b) = Booleano $ a < b
do_less_op (Flutuante a) (Flutuante b) = Booleano $ a < b
do_less_op (Inteiro a) (Flutuante b) = Booleano $ fromIntegral a < b
do_less_op (Flutuante a) (Inteiro b) = Booleano $ a < fromIntegral b

do_less_eq_op :: Type -> Type -> Type
do_less_eq_op (Inteiro a) (Inteiro b) = Booleano $ a <= b
do_less_eq_op (Flutuante a) (Flutuante b) = Booleano $ a <= b
do_less_eq_op (Inteiro a) (Flutuante b) = Booleano $ fromIntegral a <= b
do_less_eq_op (Flutuante a) (Inteiro b) = Booleano $ a <= fromIntegral b

add_sub_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
add_sub_op =
  (do
    op <- addToken;
    return do_add_op
  )
  <|>
  (do
    op <- subToken;
    return do_sub_op
  )

do_add_op :: Type -> Type -> Type
do_add_op (Inteiro a) (Inteiro b) = Inteiro $ a + b
do_add_op (Flutuante a) (Inteiro b) = Flutuante $ a + fromIntegral b
do_add_op (Inteiro a) (Flutuante b) = Flutuante $ fromIntegral a + b
do_add_op (Flutuante a) (Flutuante b) = Flutuante $ a + b
do_add_op (Str a) (Str b) = Str $ a ++ b
do_add_op (Str a) b = Str $ a ++ type_to_string b
do_add_op a (Str b) = Str $ type_to_string a ++ b
do_add_op _ _ = error "tipo inesperado"

do_sub_op :: Type -> Type -> Type
do_sub_op (Inteiro a) (Inteiro b) = Inteiro $ a - b
do_sub_op (Flutuante a) (Inteiro b) = Flutuante $ a - fromIntegral b
do_sub_op (Inteiro a) (Flutuante b) = Flutuante $ fromIntegral a - b
do_sub_op (Flutuante a) (Flutuante b) = Flutuante $ a - b
do_sub_op _ _ = error "tipo inesperado"

mul_div_rem_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
mul_div_rem_op =
  (do
    op <- mulToken;
    return do_mul_op
  )
  <|>
  (do
    op <- divToken;
    return do_div_op
  )
  <|>
  (do
    op <- remToken;
    return do_rem_op
  )

do_mul_op :: Type -> Type -> Type
do_mul_op (Inteiro a) (Inteiro b) = Inteiro $ a * b
do_mul_op (Flutuante a) (Inteiro b) = Flutuante $ a * fromIntegral b
do_mul_op (Inteiro a) (Flutuante b) = Flutuante $ fromIntegral a * b
do_mul_op (Flutuante a) (Flutuante b) = Flutuante $ a * b

do_div_op :: Type -> Type -> Type
do_div_op (Inteiro a) (Inteiro b) = Inteiro $ a `div` b
do_div_op (Flutuante a) (Inteiro b) = Flutuante $ a / fromIntegral b
do_div_op (Inteiro a) (Flutuante b) = Flutuante $ fromIntegral a / b
do_div_op (Flutuante a) (Flutuante b) = Flutuante $ a / b

do_rem_op :: Type -> Type -> Type
do_rem_op (Inteiro a) (Inteiro b) = Inteiro $ a `mod` b

pow_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
pow_op = do
  op <- powToken
  return do_pow_op

do_pow_op :: Type -> Type -> Type
do_pow_op (Inteiro a) (Inteiro b) = Inteiro $ a ^ b
do_pow_op (Flutuante a) (Inteiro b) = Flutuante $ a ** fromIntegral b
do_pow_op (Inteiro a) (Flutuante b) = Flutuante $ fromIntegral a ** b
do_pow_op (Flutuante a) (Flutuante b) = Flutuante $ a ** b

-- operador unário
unary_ops :: ParsecT [Token] MemoryState IO (Type -> Type)
unary_ops =
    (do
    op <- subToken;
    return do_neg_op
    )
    <|>
    (do
    op <- notToken;
    return do_not_op
    )

do_neg_op :: Type -> Type
do_neg_op (Inteiro a) = Inteiro $ -a
do_neg_op (Flutuante a) = Flutuante $ -a

do_not_op :: Type -> Type
do_not_op (Booleano a) = Booleano $ not a

prioritary_comparison_ops :: ParsecT [Token] MemoryState IO Token
prioritary_comparison_ops =
  try greaterToken
  <|> try greaterEqToken
  <|> try lessToken
  <|> try lessEqToken

non_prioritary_comparison_ops :: ParsecT [Token] MemoryState IO Token
non_prioritary_comparison_ops =
  try prioritary_comparison_ops
  <|> try equalToken
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
  first <- cond_if
  next <- cond_else
  return (first ++ next)

cond_if :: ParsecT [Token] MemoryState IO [Token]
cond_if = do
  exec <- executing <$> getState
  a <- ifToken
  b <- parenLeftToken
  cond <- expression
  d <- parenRightToken
  when (cond == Booleano False) $
    updateState (\st -> st { executing = False })
  e <- bracketLeftToken
  f <- stmts
  g <- bracketRightToken
  updateState (\st -> st { executing = exec })
  return [a]
  -- return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g])

cond_else :: ParsecT [Token] MemoryState IO [Token]
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
  get_type a <$> getState

all_possible_type_tokens :: ParsecT [Token] MemoryState IO Type
all_possible_type_tokens =
  try (do
    a <- try idToken <|> try typeToken
    get_type a <$> getState
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
  get_type a <$> getState

boolean_tokens :: ParsecT [Token] MemoryState IO Type
boolean_tokens = do
  a <- boolToken
  get_type a <$> getState

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
type_to_string (Inteiro x) = show x
type_to_string (Flutuante x) = show x
type_to_string (Str x) = x
type_to_string (Booleano True) = "true"
type_to_string (Booleano False) = "false"
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
  return $ Inteiro n

all_escapes_stmts :: ParsecT [Token] MemoryState IO [Token]
all_escapes_stmts = do
  a <- continueToken <|> leaveToken <|> breakToken
  b <- semiColonToken
  return (a : [b])

-- funções para a tabela de símbolos

get_type :: Token -> MemoryState -> Type
get_type (Type "int" _) _ = Inteiro 0
get_type (Type "string" _) _ = Str ""
get_type (Type "float" _) _ = Flutuante 0.0
get_type (Type "bool" _) _ = Booleano False
get_type (Int x _) _ = Inteiro x
get_type (String x _) _ = Str x
get_type (Float x _) _ = Flutuante x
get_type (Bool x _) _ = Booleano x
get_type token@(Id x _) (MemoryState _ table _) = lookup_type token table

get_variable :: Token -> MemoryState -> Type
get_variable token@(Id x _) (MemoryState table _ _) = lookup_variable token table

lookup_variable :: Token -> [(String, Type)] -> Type
lookup_variable (Id name (line, column)) [] = error $ "undefined variable \"" ++ name ++ "\": line " ++ show line ++ " column " ++ show column
lookup_variable token@(Id name _) ((name2, typ) : rest)
    | name == name2 = typ
    | otherwise = lookup_variable token rest

lookup_type :: Token -> [Type] -> Type
lookup_type (Id name (line, column)) [] = error $ "undefined type \"" ++ name ++ "\": line " ++ show line ++ " column " ++ show column
lookup_type token@(Id name _) (t@(Registro name2 _) : rest)
    | name == name2 = t
    | otherwise = lookup_type token rest

typetable_insert :: Type -> MemoryState -> MemoryState
typetable_insert t@(Registro current_name fields) st@(MemoryState _ table _) = do
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

print_symtable :: ParsecT [Token] MemoryState IO ()
print_symtable = do
  st <- getState
  liftIO $ putStrLn ("symtable: " ++ show (symtable st) ++ "\n")

print_types :: ParsecT [Token] MemoryState IO ()
print_types = do
  st <- getState
  liftIO $ putStrLn ("types: " ++ show (typetable st) ++ "\n")

-- invocação do parser para o símbolo de partida

parser :: [Token] -> IO (Either ParseError ())
parser = runParserT program (MemoryState { symtable = [] , typetable = [], executing = False}) "Parsing error!"

main :: IO ()
main = do
  tokens <- getTokens "programa.pe"
  result <- parser tokens
  case result of
    Left err -> print err
    Right ans -> print ans
