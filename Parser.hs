{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main (main) where

import Control.Arrow (Arrow (first))
import Control.Monad (Functor (fmap), Monad (return), when, unless, zipWithM_, forM_)
import Control.Monad.IO.Class
import GHC.Float (divideDouble)
import GHC.RTS.Flags (TraceFlags (user))
import Lexer
import Text.Parsec
import Data.Char (toLower)
import Data.List (nub, (\\), find, intersperse, intercalate, transpose)
import Control.Monad.Trans.Class (lift)
import Debug.Trace (traceShow, trace)
import System.Environment

-- nome, tipo, escopo, tempo de vida, valor, endereço
-- nome -> tabela de simbolos junto com escopo escopo#nome
-- tipo e valor -> unificados

-- tabela de subprogramas declarados
-- nome, parametros, tipo de retorno e código

-- flag se está executando
-- falsa se está em declaração de subprograma
-- verdadeira quando entra na main

-- representa o modo de passagem de parâmetro: por valor ou por referência
data PassMode = ByValue | ByReference
  deriving (Show, Eq)

-- nome do parâmetro, tipo, modo de passagem
type FormalParam = (String, Type, PassMode)

data Type = Integer Int | Floating Float | Str String | Boolean Bool | Record String [(String, Type)] |  Vector Type [Type] | Matrix Type [[Type]] (Int, Int)
    deriving (Show, Eq)

-- tipo para distinguir função e procedimento
data Subprogram = Procedure { proc_name :: String, proc_params :: [FormalParam], proc_body :: [Token] } |
  Function { func_name :: String, func_params :: [FormalParam], func_return_type :: Type, func_body :: [Token] }
  deriving (Show, Eq)

-- nome da variável/constante, tipo e valor, flag para identificar se é variável (true) ou não (constante)
type Symbol = (String, Type, Bool)

data ActivationRecord = ActivationRecord {
  local_symbols :: [[Symbol]]               -- pilha de escopos locais (escopo mais recente é o head)
} deriving (Show, Eq)

data ScopeType = GlobalScope | LocalScope
  deriving (Show, Eq)

data AccessStep = AccessId Token | AccessIndex Int | AccessIndex2D Int Int| AccessField Token
  deriving (Show)

data MemoryState = MemoryState {
  globals :: [Symbol],                -- variáveis globais
  call_stack :: [ActivationRecord],   -- pilha de registros de ativação (funções/procedimentos em execução)
  typetable :: [Type],                -- tabela de tipos
  subprogramtable :: [Subprogram],    -- tabela de funções e procedimentos declarados
  executing :: Bool,                  -- flag de execução
  in_procedure :: Bool,               -- flag usada pra validar `return`
  current_scope :: ScopeType,         -- escopo atual: global ou local
  last_parsed_function :: Maybe String  -- flag para detectar erros na tipagem do retorno
} deriving (Show, Eq)

-- parsers para os tokens

procedureToken :: ParsecT [Token] st IO Token
procedureToken = tokenPrim show update_pos get_token
  where
    get_token (Proc p) = Just (Proc p)
    get_token _ = Nothing

functionToken :: ParsecT [Token] st IO Token
functionToken = tokenPrim show update_pos get_token
  where
    get_token (Func p) = Just (Func p)
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
    get_token (Vectr p) = Just (Vectr p)
    get_token _ = Nothing

matrixToken :: ParsecT [Token] st IO Token
matrixToken = tokenPrim show update_pos get_token
  where
    get_token (Matrx p) = Just (Matrx p)
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

lengthToken :: ParsecT [Token] st IO Token
lengthToken = tokenPrim show update_pos get_token
  where
    get_token (Length p) = Just (Length p)
    get_token _ = Nothing

countRowsToken :: ParsecT [Token] st IO Token
countRowsToken = tokenPrim show update_pos get_token
  where
    get_token (CountRows p) = Just (CountRows p)
    get_token _ = Nothing

countColsToken :: ParsecT [Token] st IO Token
countColsToken = tokenPrim show update_pos get_token
  where
    get_token (CountCols p) = Just (CountCols p)
    get_token _ = Nothing

referenceToken :: ParsecT [Token] st IO Token
referenceToken = tokenPrim show update_pos get_token
  where
    get_token (Reference p) = Just (Reference p)
    get_token _ = Nothing

pushToken :: ParsecT [Token] st IO Token
pushToken = tokenPrim show update_pos get_token
  where
    get_token (Push p) = Just (Push p)
    get_token _ = Nothing

removeToken :: ParsecT [Token] st IO Token
removeToken = tokenPrim show update_pos get_token
  where
    get_token (Remove p) = Just (Remove p)
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (next : _) = incSourceColumn pos 1 -- avança um token
update_pos pos _ [] = pos -- fim do código-fonte

-- parsers para os não-terminais

program :: ParsecT [Token] MemoryState IO ()
program = do
  a <- initial_declarations
  b <- m
  eof
  return ()

initial_declarations :: ParsecT [Token] MemoryState IO [Token]
initial_declarations =
  try (do
    a <- struct
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
    a <- subprograms
    initial_declarations
  )
  <|> return []

struct :: ParsecT [Token] MemoryState IO [Token]
struct = do
  s <- getState
  a@(Struct (line, col)) <- structToken
  b <- idToken
  c <- bracketLeftToken
  d <- user_defined_types_declarations
  e <- bracketRightToken
  putState s
  let Id b_name _ = b in do
    updateState (typetable_insert (Record b_name d) (line, col))
    return ([a] ++ [b] ++ [c] ++ [e])

user_defined_types_declarations :: ParsecT [Token] MemoryState IO [(String, Type)]
user_defined_types_declarations = do
  first <- try user_defined_types_variable_declarations
  next <- remaining_user_defined_types_declarations
  return (first : next)

remaining_user_defined_types_declarations :: ParsecT [Token] MemoryState IO [(String, Type)]
remaining_user_defined_types_declarations =
  (do
    first <- try user_defined_types_variable_declarations
    rest <- remaining_user_defined_types_declarations
    return (first : rest)
  )
  <|> return []

user_defined_types_variable_declarations :: ParsecT [Token] MemoryState IO (String, Type)
user_defined_types_variable_declarations =
  try user_defined_types_variable_declaration
  <|> try user_defined_types_variable_declaration_assignment
  <|> try user_defined_types_variable_guess_declaration_assignment

user_defined_types_variable_declaration :: ParsecT [Token] MemoryState IO (String, Type)
user_defined_types_variable_declaration = do
  a@(Id name (line, col)) <- idToken
  b <- all_possible_type_tokens
  c <- semiColonToken
  return (name, b)

user_defined_types_variable_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type)
user_defined_types_variable_declaration_assignment = do
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
      return (name, d)
    else error $ variable_type_error_msg name b d (line, col)

user_defined_types_variable_guess_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type)
user_defined_types_variable_guess_declaration_assignment = do
  a@(Id name (line, col)) <- idToken
  b <- guessToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  s <- getState
  return (name, d)

variable_declarations :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_declarations =
  try variable_declaration
  <|> try variable_declaration_assignment
  <|> try variable_guess_declaration_assignment

variable_declaration :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_declaration = do
  a@(Id name (line, col)) <- idToken
  b <- all_possible_type_tokens
  c <- semiColonToken
  updateState (insert_symbol (name, b, True) (line, col))
  return (name, b, True)

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
      updateState (insert_symbol (name, d, True) (line, col))
      return (name, d, True)
    else error $ variable_type_error_msg name b d (line, col)

variable_guess_declaration_assignment :: ParsecT [Token] MemoryState IO (String, Type, Bool)
variable_guess_declaration_assignment = do
  a@(Id name (line, col)) <- idToken
  b <- guessToken
  c <- assignToken
  d <- expression
  e <- semiColonToken
  s <- getState
  updateState (insert_symbol (name, d, True) (line, col))
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
        updateState (insert_symbol (name, e, False) (line, col))
        return (name, e, False)
        else
          error $ const_guess_declaration_assignment_error_msg (line, col)
    else
      error $ variable_type_error_msg name declared_base_type expr_base_type (line, col)

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
    updateState (insert_symbol (name, e, False) (line, col))
    let Id name _ = a
      in return (name, e, False)
    else
      error $ const_guess_declaration_assignment_error_msg (line, col)

m :: ParsecT [Token] MemoryState IO [Token]
m = do
  s <- getState
  let mainAR = ActivationRecord { local_symbols = [[]] }
  putState s {current_scope = LocalScope, executing = True, call_stack = mainAR : call_stack s }
  a <- procedureToken
  b@(Main (line, col)) <- mainToken
  c <- parenLeftToken
  d <- params
  e <- parenRightToken
  f <- bracketLeftToken
  g <- stmts
  case g of
    Just typ -> error $ "\"main\" procedure does not expect return statements, but found return type " ++ show_pretty_type_values typ ++ "; line: "
      ++ show line ++ ", column: " ++ show col
    Nothing -> pure ()
  h <- bracketRightToken
  return [a]

subprograms :: ParsecT [Token] MemoryState IO [Token]
subprograms = try function <|> try procedure

procedure :: ParsecT [Token] MemoryState IO [Token]
procedure = do
  original_state <- getState
  let flaggedState = original_state { in_procedure = True }
  let newAR = ActivationRecord { local_symbols = [[]] }
  putState flaggedState { call_stack = newAR : call_stack flaggedState, current_scope = LocalScope }

  a <- procedureToken
  b@(Id name (line, col)) <- idToken
  c@(ParenLeft (paramline, paramcol)) <- parenLeftToken
  d <- params

  mapM_ (\(param_name, typ, passmode) -> do
            st <- getState
            let sym = (param_name, typ, True)
            updateState $ local_insert sym (line, col)
        ) d

  updateState $ subprogramtable_insert (Procedure name d []) (line, col)

  e <- parenRightToken
  f <- bracketLeftToken
  before <- getInput
  g <- stmts
  after <- getInput
  h <- bracketRightToken
  st1 <- getState

  let cs = call_stack st1
  case cs of
    [] -> error "call_stack unexpectedly empty when exiting procedure"
    (_ar : rest) -> putState $ st1 { call_stack = rest, in_procedure = False, current_scope = GlobalScope }

  -- registra o procedure na subprogramtable
  let body_tokens = take (length before - length after) before
  updateState $ subprogramtable_update (Procedure name d body_tokens)
  return [a]

function :: ParsecT [Token] MemoryState IO [Token]
function = do
  original_state <- getState
  let newAR = ActivationRecord { local_symbols = [[]] }
  putState original_state { call_stack = newAR : call_stack original_state, current_scope = LocalScope }

  a <- functionToken
  b@(Id name (line, col)) <- idToken
  c@(ParenLeft (paramline, paramcol)) <- parenLeftToken
  d <- params

  mapM_ (\(tok, typ, pass) -> updateState (local_insert (tok, typ, True) (line, col))) d

  e <- parenRightToken
  f <- all_possible_type_tokens
  g <- bracketLeftToken

  updateState (subprogramtable_insert (Function name d f []) (line, col))

  before <- getInput
  st0 <- getState
  putState st0 { last_parsed_function = Just name }
  h <- stmts
  after <- getInput
  j <- bracketRightToken
  st1 <- getState
  let cs = call_stack st1

  case cs of
    [] -> error "internal error: call_stack unexpectedly empty when exiting function!"
    (_ar : rest) -> putState $ st1 { call_stack = rest, current_scope = GlobalScope }

  case h of
    Nothing -> error $ "function \"" ++ name ++ "\" expects more return statements than provided; line: " ++ show line ++ ", column " ++ show col
    Just t -> when (extract_base_type t /= extract_base_type f) $ error $ "type mismatch in return of function \"" ++ name ++ "\": expected " ++
      show_pretty_types f ++ ", but got " ++ show_pretty_type_values t ++ "; line: " ++ show line ++ ", column " ++ show col

  let body_tokens = take (length before - length after) before
  updateState (subprogramtable_update (Function name d f body_tokens))
  st_final <- getState
  putState st_final { last_parsed_function = Just name }
  return [a]

param :: ParsecT [Token] MemoryState IO FormalParam
param =
  try (do
    a <- referenceToken
    b <- idToken
    c <- all_possible_type_tokens
    let Id b_name _ = b in
      return (b_name, c, ByReference)
  )
  <|>
  try (do
    a <- idToken
    b <- all_possible_type_tokens
    let Id a_name _ = a in
      return (a_name, b, ByValue)
  )

params :: ParsecT [Token] MemoryState IO [FormalParam]
params =
  try (do
    first <- param
    next <- remainingParams
    return (first : next)
  )
  <|> return []

remainingParams :: ParsecT [Token] MemoryState IO [FormalParam]
remainingParams =
  try (do
    a <- commaToken
    b <- param
    c <- remainingParams
    return (b : c)
  )
  <|> return []

stmts :: ParsecT [Token] MemoryState IO (Maybe Type)
stmts = do
  first <- stmt
  exec <- executing <$> getState
  
  case first of
    Just n -> updateState (\st -> st { executing = False })
    Nothing -> return ()

  next <- remaining_stmts
  updateState (\st -> st { executing = exec })

  case first of
    Just n -> return first
    Nothing -> return next

remaining_stmts :: ParsecT [Token] MemoryState IO (Maybe Type)
remaining_stmts =
  try (do
    a <- stmt
    exec <- executing <$> getState

    case a of
      Just n -> updateState (\st -> st { executing = False })
      Nothing -> return ()

    next <- remaining_stmts
    updateState (\st -> st { executing = exec })

    case a of
      Just n -> return a
      Nothing -> return next
  )
  <|> return Nothing

stmt :: ParsecT [Token] MemoryState IO (Maybe Type)
stmt =
  try loop
  <|> try conditional
  <|>
  try (do
    procedure_call
    return Nothing
  )
  <|>
  try (do
    variable_declarations
    return Nothing
  )
  <|>
  try (do
    const_declarations
    return Nothing
  )
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
    return do_number_mul_op
  )
  <|>
  try (do
    DivAssign pos@(line, col) <- divAssignToken
    s <- getState
    return (do_div_op s)
  )
  <|>
  try (do
    RemAssign pos@(line, col) <- remAssignToken
    s <- getState
    return (do_rem_op s)
  )
  <|>
  try (do
    PowAssign pos@(line, col) <- powAssignToken
    return do_pow_op
  )

assign_eq :: (Int, Int) -> Type -> Type -> Type
assign_eq _ assignee assigned = assigned -- retorna o valor original

assign :: ParsecT [Token] MemoryState IO (Maybe Type)
assign = try $ do
  (access_type, token_chain) <- access_chain
  assign_function <- all_assign_tokens
  b <- expression
  _ <- semiColonToken
  s <- getState

  maybe_cons <- case token_chain of
    (AccessId tok@(Id _ pos)) : _ -> return tok
    _ -> error "internal error: expected access_id as the first access step!"

  -- verifica se o access_chain é uma constante
  let (Id name (line, col)) = maybe_cons
  maybe_const <- is_name_constant maybe_cons

  case maybe_const of
    Just True -> error $ "cannot assign to constant \"" ++ name ++ "\"; line: " ++ show line ++ ", column: " ++ show col
    Just False -> return ()
    Nothing -> error "internal error: name does not exist!"

  let access_base_type = extract_base_type access_type
      expr_base_type = extract_base_type b
      new_value = assign_function (line, col) access_type b

  if access_base_type == expr_base_type
    then do
      when (executing s) $ do
        updateState (symtable_update_by_access_chain token_chain new_value)
      return Nothing
    else
      error $ assign_type_error_msg access_type b (line, col)

procedure_call :: ParsecT [Token] MemoryState IO ()
procedure_call = try $ do
  Id proc_name (line, col) <- idToken
  lookAhead parenLeftToken
  st <- getState
  let maybe_proc = find (\sp -> case sp of
                                  Procedure name _ _ -> name == proc_name
                                  _ -> False) (subprogramtable st)

  case maybe_proc of
    Nothing -> error $ "procedure \"" ++ proc_name ++ "\" not declared; line " ++ show line ++ " column " ++ show col
    Just (Procedure _ params body_tokens) -> do
      parenLeftToken
      args <- actual_params <|> return []
      parenRightToken
      semiColonToken

      when (length args /= length params) $
        error $ "incorrect number of arguments in call to procedure \"" ++ proc_name ++
          "\": expected " ++ show (length params) ++ ", got " ++ show (length args) ++
          "; line: " ++ show line ++ ", column: " ++ show col

      zipWithM_ (\(formal_name, formal_type, passmode) actual -> do
        case passmode of
          ByReference ->
            case actual of
              Left (t, access_steps) ->
                case access_steps of
                  (AccessId tokFirst : _) -> do
                    mIsConst <- is_name_constant tokFirst
                    case mIsConst of
                      Just True ->
                        error $ "actual parameter \"" ++ get_id_name tokFirst ++
                          "\" is declared as constant and cannot be passed by reference; line " ++ show line ++ ", column: " ++ show col
                      _ -> return ()
                  _ -> return ()  -- não começa com access_id
              Right _ ->
                error $ "no literals may be used as by-reference parameters; line " ++ show line ++ ", column: " ++ show col
          ByValue -> return ()
        ) params args

      let new_ar = ActivationRecord { local_symbols = [[]] }
      let st' = st { call_stack = new_ar : call_stack st }
      putState st'

      zipWithM_ (\(formal_name, formal_type, _) actual -> do
        let actual_type = case actual of
              Left (t, _) -> t
              Right t     -> t

        when (extract_base_type formal_type /= extract_base_type actual_type) $
          error $ "type mismatch for parameter \"" ++ formal_name ++
            "\" in call to function \"" ++ proc_name ++ "\": expected " ++ show_pretty_types formal_type ++
            ", got " ++ show_pretty_type_values actual_type ++ "; line: " ++ show line ++ ", column: " ++ show col

        updateState (insert_symbol (formal_name, actual_type, True) (line, col))
        ) params args


      s' <- getState
      new_state <- if executing s'
        then do
          result <- lift $ runParserTWithState stmts s' body_tokens
          case result of
            Left err -> error (show err)
            Right (_, st') -> return st'
        else do
          return s'

      case call_stack s' of
        [] -> error "internal error: call_stack unexpectedly empty after procedure call!"
        (_ : rest) -> putState $ new_state { call_stack = rest }

      zipWithM_ (\(name, _, passmode) param -> do
        case (passmode, param) of
          (ByValue, _) -> return ()
          (ByReference, Right _) -> return ()
          (ByReference, Left (_, toks)) -> do
            updateState $
              symtable_update_by_access_chain toks
                (lookup_variable (Id name (line, col)) new_state)
        ) params args
  <|> try native_procedure_call

native_procedure_call :: ParsecT [Token] MemoryState IO ()
native_procedure_call =
  try print_procedure
  <|> try vector_push_procedure
  <|> try vector_remove_procedure

vector_push_procedure :: ParsecT [Token] MemoryState IO ()
vector_push_procedure = do
  s <- getState
  Push (line, col) <- pushToken
  _ <- parenLeftToken
  vec@(t_ac, as_ac) <- access_chain
  _ <- commaToken
  expr <- expression
  _ <- parenRightToken
  _ <- semiColonToken

  case t_ac of
    Vector t elements -> 
      if extract_base_type expr == t
        then when (executing s) $
            updateState (symtable_update_by_access_chain as_ac (Vector t (elements ++ [expr])))
        else error $ "type mismatch: cannot add value of type " ++ show_pretty_type_values expr ++
          " to vector of " ++ show_pretty_types t ++ "; line " ++ show line ++ ", column " ++ show col
    _ -> error $ "cannot apply push function to non-vector variables; line " ++ show line ++ ", column " ++ show col

vector_remove_procedure :: ParsecT [Token] MemoryState IO ()
vector_remove_procedure = do
  s <- getState
  Remove (line, col) <- removeToken
  _ <- parenLeftToken
  vec@(t_ac, as_ac) <- access_chain
  _ <- parenRightToken
  _ <- semiColonToken

  case t_ac of
    Vector t elements ->
      when (executing s) $
        if null elements
            then error $ "cannot remove element from empty vector; line " ++ show line ++ ", column " ++ show col
            else updateState (symtable_update_by_access_chain as_ac (Vector t (init elements)))
    _ -> error $ "cannot apply remove function to non-vector variables; line " ++ show line ++ ", column " ++ show col

function_return :: ParsecT [Token] MemoryState IO (Maybe Type)
function_return = do
  a@(Return (line, col)) <- returnToken
  st <- getState
  when (in_procedure st) $
    error $ "return statements are not allowed among procedures; line: " ++ show line ++ ", column: " ++ show col
  b <- expression

  when (not (executing st)) $
    case last_parsed_function st of
      Just name -> do
        let maybe_func = find (\sp -> case sp of
                                        Function fname _ _ _ -> fname == name
                                        _ -> False)
                              (subprogramtable st)
        case maybe_func of 
          Just (Function of_name _ return_type _) ->
            if extract_base_type return_type /= extract_base_type b
              then error $ "type error: function \"" ++ of_name ++ "\" expects return type " ++ show_pretty_types return_type ++
                ", but got " ++ show_pretty_types b ++ "; line: " ++ show line ++ ", column: " ++ show col
              else return ()
          _ -> error $ "function not found in subprogram table; line: " ++ show line ++ ", column: " ++ show col

      _ -> error $ "no function context for return statement; line: " ++ show line ++ ", column: " ++ show col

  c <- semiColonToken
  return (Just b)


expressions :: ParsecT [Token] MemoryState IO [Type]
expressions = do
  first <- expression
  next <- remaining_expressions
  return (first : next)

remaining_expressions :: ParsecT [Token] MemoryState IO [Type]
remaining_expressions =
  try (do
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
  )

-- operadores binários
or_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
or_op = do
  Or (line, col) <- orToken
  return (\a b -> case (a, b) of
    (Boolean x, Boolean y) -> Boolean (x || y)
    (x, y) -> error $ binary_type_error "or" x y (line, col))

and_op :: ParsecT [Token] MemoryState IO (Type -> Type -> Type)
and_op = do
  And (line, col) <- andToken;
  return (\a b -> case (a, b) of
    (Boolean x, Boolean y) -> Boolean (x && y)
    (x, y) -> error $ binary_type_error "and" x y (line, col))

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
  -- numérico
  (Integer x, Integer y) -> Integer $ x + y
  (Floating x, Integer y) -> Floating $ x + fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x + y
  (Floating x, Floating y) -> Floating $ x + y
  -- string
  (Str x, Str y) -> Str $ x ++ y
  (Str x, y) -> Str $ x ++ type_to_string y
  (x, Str y) -> Str $ type_to_string x ++ y
  -- matriz + matriz
  (Matrix t1 elements1 (rows1, cols1), Matrix t2 elements2 (rows2, cols2))
    | t1 /= t2 -> error $ binary_type_error "+" a b (line, col)
    | rows1 /= rows2 || cols1 /= cols2 ->error $ "size error: matrices with different dimensions cannot be added; line " ++ show line ++ ", column " ++ show col
    | otherwise -> Matrix t1 (zipWith (zipWith (do_add_op (line, col))) elements1 elements2) (rows1, cols1)
  -- fallback
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
    s <- getState
    return (do_div_op s (line, col))
  )
  <|>
  (do
    Rem (line, col) <- remToken;
    s <- getState
    return (do_rem_op s (line, col))
  )

do_mul_op :: (Int, Int) -> (Type -> Type -> Type)
do_mul_op (line, column) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x * y
  (Floating x, Integer y) -> Floating $ x * fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x * y
  (Floating x, Floating y) -> Floating $ x * y
  -- matrix multiplication
  (Matrix t1 aRows (rows1, cols1), Matrix t2 bRows (rows2, cols2))
    | t1 /= t2 -> error $ binary_type_error "*" a b (line, column)
    | cols1 /= rows2 ->
        error $ "size error: incompatible matrix dimensions for multiplication; line " ++ show line ++ ", column " ++ show column
    | otherwise ->
        let result = [[ sum_row_col row colVec | colVec <- transpose bRows] | row <- aRows]
        in Matrix t1 result (rows1, cols2)
    where
      sum_row_col :: [Type] -> [Type] -> Type
      sum_row_col row colVec =
        foldl1 (do_add_op (line, column)) $ zipWith (do_mul_op (line, column)) row colVec
  (x, y) -> error $ binary_type_error "*" x y (line, column)

do_number_mul_op :: (Int, Int) -> (Type -> Type -> Type)
do_number_mul_op (line, col) = \a b -> case (a, b) of
  (Integer x, Integer y) -> Integer $ x * y
  (Floating x, Integer y) -> Floating $ x * fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x * y
  (Floating x, Floating y) -> Floating $ x * y
  (x, y) -> error $ binary_type_error "*" x y (line, col)

do_div_op :: MemoryState -> (Int, Int) -> (Type -> Type -> Type)
do_div_op st (line, col) = \a b -> case (a, b) of
  (Integer _, Integer 0) -> 
    if (executing st)
      then error $ "no mathematical divisions per 0 are allowed; line: " ++ show line ++ "; column: " ++ show col
      else Integer 0
  (Floating _, Integer 0) ->
    if (executing st)
      then error $ "no mathematical divisions per 0 are allowed; line: " ++ show line ++ "; column: " ++ show col
      else Floating 0.0
  (Integer _, Floating 0.0) -> 
    if (executing st)
      then error $ "no mathematical divisions per 0.0 are allowed; line: " ++ show line ++ "; column: " ++ show col
      else Floating 0.0
  (Floating _, Floating 0.0) ->
    if (executing st)
      then error $ "no mathematical divisions per 0.0 are allowed; line: " ++ show line ++ "; column: " ++ show col
      else Floating 0.0
  (Integer x, Integer y) -> Integer $ x `div` y
  (Floating x, Integer y) -> Floating $ x / fromIntegral y
  (Integer x, Floating y) -> Floating $ fromIntegral x / y
  (Floating x, Floating y) -> Floating $ x / y
  (x, y) -> error $ binary_type_error "/" x y (line, col)

do_rem_op :: MemoryState -> (Int, Int) -> (Type -> Type -> Type)
do_rem_op st (line, col) = \a b -> case (a, b) of
  (Integer _, Integer 0) -> 
    if (executing st)
      then error $ "no mathematical divisions per 0 are allowed; line: " ++ show line ++ "; column: " ++ show col
      else Integer 0
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

access_chain :: ParsecT [Token] MemoryState IO (Type, [AccessStep])
access_chain = do
  first_id@(Id name (line, col)) <- idToken
  st <- getState
  let current_type = lookup_variable first_id st
  (final_type, chain_steps) <- access_chain_tail name current_type (line, col)
  return (final_type, AccessId first_id : chain_steps)

access_chain_tail :: String -> Type -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, [AccessStep])
access_chain_tail varName current_type pos = do
  next <- optionMaybe (lookAhead anyToken)
  case next of
    Just (Dot _)        -> user_defined_types_field_access varName current_type pos >>= continue
    Just (BraceLeft _)  -> indexed_access varName current_type pos >>= continue
    _                   -> return (current_type, [])
  where
    continue (new_type, steps) = do
      (final_type, rest) <- access_chain_tail varName new_type pos
      return (final_type, steps ++ rest)

indexed_access :: String -> Type -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, [AccessStep])
indexed_access varName (Vector t elems) (line, col) = do
  s <- getState
  _ <- braceLeftToken
  index_expr <- expression
  _ <- braceRightToken
  case index_expr of
    Integer i ->
      if executing s 
        then
          if i < 0 || i >= length elems
            then error $ "index " ++ show i ++ " out of bounds for vector's length; line " ++ show line ++ ", column " ++ show col
            else return (elems !! i, [AccessIndex i])
        else
          return (t, [AccessIndex i])
    _ -> error $ "index must be an integer for vector access; line " ++ show line ++ ", column " ++ show col
indexed_access varName (Matrix t elements (rows, cols)) (line, col) = do
  _ <- braceLeftToken
  i_expr <- expression
  _ <- commaToken
  j_expr <- expression
  _ <- braceRightToken
  case (i_expr, j_expr) of
    (Integer i, Integer j) ->
      if i < 0 || i >= rows
        then error $ "row index access at " ++ show i ++ " is out of bounds for matrix; line " ++ show line ++ ", column " ++ show col
      else if j < 0 || j >= cols
        then error $ "column index acess at " ++ show j ++ " is out of bounds for matrix; line " ++ show line ++ ", column " ++ show col
      else return ((elements !! i) !! j, [AccessIndex2D i j])
    _ -> error $ "both indices must be integers for matrix access; line " ++ show line ++ ", column " ++ show col
indexed_access _ t (line, col) =
  error $ "type " ++ show_pretty_type_values t ++ " does not support indexed access; line " ++ show line ++ ", column " ++ show col

user_defined_types_field_access :: String -> Type -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, [AccessStep])
user_defined_types_field_access varName (Record name fields) pos = do
  (field_type, field_token) <- access_from_user_defined_types_fields name fields pos
  (final_type, rest) <- access_chain_tail varName field_type pos
  return (final_type, AccessField field_token : rest)
user_defined_types_field_access _ t (line, col) =
  error $ "type " ++ show_pretty_type_values t ++ " does not support field access; line " ++ show line ++ ", column " ++ show col

access_from_user_defined_types_fields :: String -> [(String, Type)] -> (Int, Int) -> ParsecT [Token] MemoryState IO (Type, Token)
access_from_user_defined_types_fields type_name entries _ = do
  _ <- dotToken
  field@(Id field_name (line, col)) <- idToken
  case lookup field_name entries of
    Just t  -> return (t, field)
    Nothing -> error $ "cannot access field \"" ++ field_name ++ "\" on value of type " ++ type_name ++ "; line " ++ show line ++ ", column " ++ show col

function_call :: ParsecT [Token] MemoryState IO Type
function_call = try $ do
  Id function_name (line, col) <- idToken
  lookAhead parenLeftToken
  st <- getState
  let maybe_func = find (\sp -> case sp of
                                  Function name _ _ _ -> name == function_name
                                  _ -> False) (subprogramtable st)

  case maybe_func of
    Nothing -> error $ "function \"" ++ function_name ++ "\" not declared; line " ++ show line ++ " column " ++ show col
    Just (Function _ params return_type body_tokens) -> do
      parenLeftToken
      args <- actual_params <|> return []
      parenRightToken

      when (length args /= length params) $
        error $ "incorrect number of arguments in call to function \"" ++ function_name ++
          "\": expected " ++ show (length params) ++ ", got " ++ show (length args) ++
          "; line: " ++ show line ++ ", column: " ++ show col

      zipWithM_ (\(formal_name, formal_type, passmode) actual -> do
        case passmode of
          ByReference ->
            case actual of
              Left (t, access_steps) ->
                case access_steps of
                  (AccessId tokFirst : _) -> do
                    mIsConst <- is_name_constant tokFirst
                    case mIsConst of
                      Just True ->
                        error $ "actual parameter \"" ++ get_id_name tokFirst ++
                          "\" is declared as constant and cannot be passed by reference; line " ++ show line ++ ", column: " ++ show col
                      _ -> return ()
                  _ -> return ()  -- não começa com access_id
              Right _ ->
                error $ "no literals may be used as by-reference parameters; line " ++ show line ++ ", column: " ++ show col
          ByValue -> return ()
        ) params args

      let new_ar = ActivationRecord { local_symbols = [[]] }
      let st' = st { call_stack = new_ar : call_stack st }
      putState st'

      zipWithM_ (\(formal_name, formal_type, _) actual -> do
        let actual_type = case actual of
              Left (t, _) -> t
              Right t     -> t

        when (extract_base_type formal_type /= extract_base_type actual_type) $
          error $ "type mismatch for parameter \"" ++ formal_name ++
            "\" in call to function \"" ++ function_name ++ "\": expected " ++ show_pretty_types formal_type ++ ", got " ++
            show_pretty_type_values actual_type ++ "; line: " ++ show line ++ ", column: " ++ show col

        updateState (insert_symbol (formal_name, actual_type, True) (line, col))
        ) params args
      
      s' <- getState
      (ret, new_state) <- if executing s'
        then do
          result <- lift $ runParserTWithState stmts s' body_tokens

          case result of
            Left err -> error (show err)
            Right (Just return_val, new_state) ->
              if compare_type_base return_type return_val
                then return (return_val, new_state)
                else error $ "type error: function \"" ++ function_name ++ "\" expects return type " ++ show_pretty_types return_type ++
                ", but got " ++ show_pretty_type_values return_val ++ "; line: " ++ show line ++ ", column: " ++ show col
            Right (Nothing, _) -> error $ "function \"" ++ function_name ++ "\" expects more return statements than provided; line: " ++ show line ++ ", column: " ++ show col
        else do
          return (return_type, s')

      case call_stack s' of
        [] -> error "call_stack unexpectedly empty after procedure call"
        (_ : rest) -> putState $ new_state { call_stack = rest }

      zipWithM_ (\(name, _, passmode) param -> do
        case (passmode, param) of
          (ByValue, _) -> return ()
          (ByReference, Right _) -> return ()
          (ByReference, Left (_, toks)) -> do
            updateState $
              symtable_update_by_access_chain toks
                (lookup_variable (Id name (line, col)) new_state)
        ) params args

      return ret
  <|> try native_function_call

actual_params :: ParsecT [Token] MemoryState IO [Either (Type, [AccessStep]) Type]
actual_params = do
  first <- actual_param
  next <- remaining_params
  return (first : next)

remaining_params :: ParsecT [Token] MemoryState IO [Either (Type, [AccessStep]) Type]
remaining_params =
  try (do
    a <- commaToken
    b <- actual_param
    c <- remaining_params
    return (b : c)
  )
  <|> return []

actual_param :: ParsecT [Token] MemoryState IO (Either (Type, [AccessStep]) Type)
actual_param =
  try (do
    a <- access_chain
    lookAhead (commaToken <|> parenRightToken)
    return $ Left a
  ) <|>
  try (do
    Right <$> expression
  )

loop :: ParsecT [Token] MemoryState IO (Maybe Type)
loop = while <|> repeat_until <|> for

while :: ParsecT [Token] MemoryState IO (Maybe Type)
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
  updateState enter_scope

  f <- stmts

  updateState exit_scope
  updateState (\st -> st { executing = exec })

  inputAfter <- getInput
  g <- bracketRightToken

  let bodyTokens = take (length inputBefore - length inputAfter) inputBefore
  if exec 
    then run_loop False condTokens bodyTokens [] (line, column) Nothing
    else return Nothing

repeat_until :: ParsecT [Token] MemoryState IO (Maybe Type)
repeat_until = do
    exec <- executing <$> getState
    a <- repeatToken
    b <- bracketLeftToken
    inputBefore <- getInput

    updateState enter_scope
    c <- stmts
    updateState exit_scope

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
    if exec 
        then run_loop True condTokens bodyTokens [] (line, column) c
        else return c

run_loop :: Bool -> [Token] -> [Token] -> [Token] -> (Int, Int) -> (Maybe Type) -> ParsecT [Token] MemoryState IO (Maybe Type)
run_loop negateCondition condTokens bodyTokens incrementTokens (line, column) previous_return = do
  updateState enter_scope

  state <- getState
  condResult <- lift $ runParserT expression state "" condTokens
  condBool <- case condResult of
    Left parseErr -> error (show parseErr)
    Right condExpr -> case check_condition "loop" (line, column) condExpr of
      Left err -> error err
      Right boolVal -> return (if negateCondition then not boolVal else boolVal)
  
  return_val <- if condBool
    then do
      bodyState <- getState
      result <- lift $ runParserTWithState stmts bodyState bodyTokens
      case result of
        Left err -> error (show err)
        Right (current_return, bodyResultState) -> do
          putState bodyResultState

          incrementState <- getState
          incrementResult <- lift $ runParserTWithState (try for_assign <|> return Nothing) incrementState incrementTokens
          case incrementResult of
            Left err -> error (show err)
            Right (_, afterIncrementState) -> putState afterIncrementState

          updateState exit_scope
          case current_return of
              Just a -> return current_return
              Nothing -> run_loop negateCondition condTokens bodyTokens incrementTokens (line, column) current_return
    else do
      updateState exit_scope
      return previous_return

  return return_val

for :: ParsecT [Token] MemoryState IO (Maybe Type)
for = do
  a <- forToken
  b@(ParenLeft (line, column)) <- parenLeftToken
  updateState enter_scope
  c <- variable_declaration_assignment
  inputBeforeCond <- getInput
  cond <- expression
  inputAfterCond <- getInput
  e <- semiColonToken

  beforeAssign <- getState
  inputBeforeAssign <- getInput
  _ <- for_assign
  inputAfterAssign <- getInput
  putState beforeAssign
  g <- parenRightToken
  h <- bracketLeftToken
  exec <- executing <$> getState
  inputBeforeBody <- getInput

  updateState (\st -> st { executing = False })
  updateState enter_scope

  body <- stmts

  updateState exit_scope
  updateState (\st -> st { executing = exec })

  inputAfterBody <- getInput
  j <- bracketRightToken

  let bodyTokens = take (length inputBeforeBody - length inputAfterBody) inputBeforeBody
  let condTokens = take (length inputBeforeCond - length inputAfterCond) inputBeforeCond
  let assignTokens = take (length inputBeforeAssign - length inputAfterAssign) inputBeforeAssign

  return_value <- if exec
    then run_loop False condTokens bodyTokens assignTokens (line, column) Nothing
    else return Nothing

  updateState exit_scope
  return return_value

for_condition :: ParsecT [Token] MemoryState IO [Token]
for_condition =
  try (do
    a <- idToken
    b <- all_comparison_ops
    c <- expression
    return [a]
  )
  <|>
  try (do
    a <- idToken
    b <- all_comparison_ops
    c <- expression
    return [a]
  )

for_assign :: ParsecT [Token] MemoryState IO (Maybe Type)
for_assign = try $ do
  (access_type, token_chain) <- access_chain
  assign_function <- all_assign_tokens
  b <- expression
  s <- getState
  let access_base_type = extract_base_type access_type
      expr_base_type = extract_base_type b
  (Id _ (line, col)) <- case token_chain of
    (AccessId tok@(Id _ pos)) : _ -> return tok
    _ -> fail "internal error: expected access_id as the first access step!"
  let new_value = assign_function (line, col) access_type b
  if access_base_type == expr_base_type
    then do
      when (executing s) $
        updateState (symtable_update_by_access_chain token_chain new_value)
      return Nothing
    else
      error $ assign_type_error_msg access_type b (line, col)

conditional :: ParsecT [Token] MemoryState IO (Maybe Type)
conditional = if_else <|> match_case

if_else :: ParsecT [Token] MemoryState IO (Maybe Type)
if_else = do
  (first, cond) <- cond_if
  (has_else, next) <- cond_else cond
  
  returns_same_type <- case (first, next) of
    (Just a, Just b) -> return $ compare_type_base a b
    (_, _) -> return False
  
  if has_else && not returns_same_type
    then return Nothing
    else if cond 
        then return first
        else return next

cond_if :: ParsecT [Token] MemoryState IO (Maybe Type, Bool)
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
  updateState enter_scope
  return_type <- stmts
  updateState exit_scope
  g <- bracketRightToken
  updateState (\st -> st { executing = exec })
  return (return_type, condBool)

check_condition :: String -> (Int, Int) -> Type  -> Either String Bool
check_condition _ _ (Boolean True) = Right True
check_condition _ _ (Boolean False) = Right False
check_condition conditional_name (line, column) t =
  Left $ "type error in \"" ++ conditional_name ++ "\" condition: expected boolean, got " ++
    show_pretty_type_values t ++ "; line: " ++ show line ++ ", column: " ++ show column

cond_else :: Bool -> ParsecT [Token] MemoryState IO (Bool, Maybe Type)
cond_else if_cond =
  (do
    exec <- executing <$> getState
    a <- elseToken
    b <- bracketLeftToken
    when if_cond $
      updateState (\st -> st { executing = False })
    updateState enter_scope
    c <- stmts
    updateState exit_scope
    d <- bracketRightToken
    updateState (\st -> st { executing = exec })
    return (True, c)
  )
  <|> return (False, Nothing)

match_case :: ParsecT [Token] MemoryState IO (Maybe Type)
match_case = do
  exec <- executing <$> getState
  Match (line, column) <- matchToken
  _ <- parenLeftToken
  expr <- expression
  _ <- parenRightToken
  _ <- bracketLeftToken
  (return_type, matched_total) <- cases expr
  h <- bracketRightToken
  return return_type

cases :: Type -> ParsecT [Token] MemoryState IO (Maybe Type, Bool)
cases expr =
  try $ default_case False
  <|>
  try (do
    (a, matched_first) <- single_case expr False
    (has_next, b, matched_total) <- remaining_cases expr matched_first

    let return_value = if matched_first then a else b

    returns_same_type <- case (a, b) of
        (Just a, Just b) -> return $ compare_type_base a b
        (_, _) -> return False

    if has_next && not returns_same_type
        then return (Nothing, matched_total)
        else return (return_value, matched_total)
  )

remaining_cases :: Type -> Bool -> ParsecT [Token] MemoryState IO (Bool, Maybe Type, Bool)
remaining_cases expr matched =
  try (do
    (return_type, matched') <- default_case matched
    return (True, return_type, matched')
  )
  <|>
  try (do
    (first, matched_current) <- single_case expr matched
    (has_next, next, matched_total) <- remaining_cases expr matched_current

    let return_value = if matched_current then first else next

    returns_same_type <- case (first, next) of
      (Just a, Just b) -> return $ compare_type_base a b
      (_, _) -> return False

    if has_next && not returns_same_type
        then return (True, Nothing, matched_total)
        else return (True, return_value, matched_total)
  )
  <|> return (False, Nothing, matched)

single_case :: Type -> Bool -> ParsecT [Token] MemoryState IO (Maybe Type, Bool)
single_case expr matched = do
  exec <- executing <$> getState
  a <- caseToken
  cond <- case_expression expr
  c <- colonToken

  when (not cond || matched) $
    updateState (\st -> st { executing = False });

  d <- stmts
  
  updateState (\st -> st { executing = exec })
  return (d, cond || matched)

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

default_case :: Bool -> ParsecT [Token] MemoryState IO (Maybe Type, Bool)
default_case matched = do
  exec <- executing <$> getState
  a <- defaultToken
  b <- colonToken
  when matched $
      updateState (\st -> st { executing = False })
  c <- stmts
  updateState (\st -> st { executing = exec })
  return (c, True)

numeric_literal_tokens :: ParsecT [Token] MemoryState IO Type
numeric_literal_tokens = do
  a <- try intToken <|> try floatToken
  get_type_value a <$> getState

all_possible_type_tokens :: ParsecT [Token] MemoryState IO Type
all_possible_type_tokens =
  try (do
    a <- try idToken <|> try typeToken
    st <- getState
    let t = get_type_value a st
    t `seq` return t
  )
  <|>
  try (do
    a <- vectorToken
    b <- lessToken
    c <- all_possible_type_tokens
    d <- greaterToken
    let t = get_vector_value a c
    t `seq` return t
  )
  <|>
  try (do
    a@(Matrx (line, col)) <- matrixToken
    b <- lessToken
    c <- all_possible_type_tokens
    _ <- colonToken
    _ <- braceLeftToken
    rows <- expression
    _ <- commaToken
    columns <- expression
    _ <- braceRightToken
    _ <- greaterToken
    let t = get_matrix_value a c (extract_matrix_dimensions (rows, columns) (line, col))
    t `seq` return t
  )

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
type_to_string (Vector _ elements) = "[" ++ elements_str ++ "]" where elements_str = intercalate ", " (map type_to_string elements)
type_to_string (Matrix _ elements (line, cols)) = "[" ++ rows_str ++ "]" where
  rows_str = intercalate ", " (map row_to_string elements)
  row_to_string row = "[" ++ intercalate ", " (map type_to_string row) ++ "]"

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

vector_length_function :: ParsecT [Token] MemoryState IO Type
vector_length_function = do
  Length (line, col) <- lengthToken
  _ <- parenLeftToken
  expr <- expression
  _ <- parenRightToken
  case expr of
    Vector _ _ -> return (Integer (measure_vector_size expr))
    _ -> error $ "cannot apply length function to non-vector variables; line " ++ show line ++ ", column " ++ show col

matrix_rows_function :: ParsecT [Token] MemoryState IO Type
matrix_rows_function = do
  CountRows (line, col) <- countRowsToken
  _ <- parenLeftToken
  expr <- expression
  _ <- parenRightToken
  case expr of
    Matrix _ _ (rows, _) -> return (Integer rows)
    _ -> error $ "cannot apply count_rows function to non-matrix variables; line " ++ show line ++ ", column " ++ show col

matrix_cols_function :: ParsecT [Token] MemoryState IO Type
matrix_cols_function = do
  CountCols (line, col) <- countColsToken
  _ <- parenLeftToken
  expr <- expression
  _ <- parenRightToken
  case expr of
    Matrix _ _ (_, cols) -> return (Integer cols)
    _ -> error $ "cannot apply count_columns function to non-matrix variables; line " ++ show line ++ ", column " ++ show col

native_function_call :: ParsecT [Token] MemoryState IO Type
native_function_call = 
  try scan_function 
  <|> try vector_length_function
  <|> matrix_rows_function
  <|> matrix_cols_function

lookup_variable :: Token -> MemoryState -> Type
lookup_variable (Id name (line, column)) st =
  -- tenta achar nas variáveis globais
  case lookup_in_table name (globals st) of
    Just typ -> typ
    Nothing -> -- não achou nas globais, tenta nas locais
      case call_stack st of
        (ar:_) -> lookup_in_activation_record_or_error name ar line column
        [] -> error_not_found name line column  -- sem call stack, erro direto

-- busca nas variáveis locais, e se não achar, lança erro
lookup_in_activation_record_or_error :: String -> ActivationRecord -> Int -> Int -> Type
lookup_in_activation_record_or_error name ar line column =
  case lookup_in_activation_record name ar of
    Just typ -> typ
    Nothing -> error_not_found name line column

-- busca numa tabela simples
lookup_in_table :: String -> [Symbol] -> Maybe Type
lookup_in_table _ [] = Nothing
lookup_in_table name ((n, typ, _):rest)
  | name == n = Just typ
  | otherwise = lookup_in_table name rest

-- busca na pilha de escopos (escopos aninhados)
lookup_in_scopes :: String -> [[Symbol]] -> Maybe Type
lookup_in_scopes _ [] = Nothing
lookup_in_scopes name (scope:rest) =
  case lookup_in_table name scope of
    Just typ -> Just typ
    Nothing -> lookup_in_scopes name rest

-- busca no activation record e sobes no static link se necessário
lookup_in_activation_record :: String -> ActivationRecord -> Maybe Type
lookup_in_activation_record name ar =
  case lookup_in_scopes name (local_symbols ar) of
    Just typ -> Just typ
    Nothing -> Nothing

lookup_const_in_activation_record :: String -> ActivationRecord -> Maybe Bool
lookup_const_in_activation_record name ar =
  case lookup_const_in_scopes name (local_symbols ar) of
      Just is_const -> Just is_const
      Nothing -> Nothing

lookup_const_in_scopes :: String -> [[Symbol]] -> Maybe Bool
lookup_const_in_scopes _ [] = Nothing
lookup_const_in_scopes name (scope:rest) =
  case find (\(n, _, _) -> n == name) scope of
      Just (_, _, is_var) -> Just (not is_var)
      Nothing -> lookup_const_in_scopes name rest

enter_scope :: MemoryState -> MemoryState
enter_scope st =
  case call_stack st of
    [] -> error "interal error: no active activation record to enter scope!"
    (ar : rest) ->
      let updated_locals = [] : local_symbols ar  -- empilha novo escopo vazio
          updatedAR = ar { local_symbols = updated_locals }
      in st { call_stack = updatedAR : rest }

exit_scope :: MemoryState -> MemoryState
exit_scope st =
  case call_stack st of
    [] -> error "interal error: no active activation record to exit scope!"
    (ar : rest) ->
      case local_symbols ar of
        [] -> error "internal error: no local scopes to exit!"
        (_ : remaining_scopes) ->
          let updatedAR = ar { local_symbols = remaining_scopes }
          in st { call_stack = updatedAR : rest }

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
get_type_value token@(Id x _) (MemoryState _ _ table _ _ _ _ _) = lookup_type token table

get_matrix_value :: Token -> Type -> (Int, Int) -> Type
get_matrix_value token inner_type (rows, cols) =
  case token of
    Matrx _ -> Matrix base_type matrix_data (rows, cols)
      where
        base_type = extract_base_type inner_type
        row = replicate cols base_type
        matrix_data = replicate rows row
    _ -> error "internal error: unknown data-structure type provided by token!"

get_vector_value :: Token -> Type -> Type
get_vector_value  token inner_type =
  case token of
    Vectr _ -> Vector (extract_base_type inner_type) []
    _       -> error "internal error: unknown data-structure type provided by token!"

is_name_constant :: Token -> ParsecT [Token] MemoryState IO (Maybe Bool)
is_name_constant (Id name _) = do
  st <- getState
  case find (\(n, _, _) -> n == name) (globals st) of
    Just (_, _, is_var) -> return $ Just (not is_var)
    Nothing -> case call_stack st of
      (ar:_) -> return $ lookup_const_in_activation_record name ar
      [] -> return Nothing
is_name_constant _ = return Nothing

lookup_type :: Token -> [Type] -> Type
lookup_type (Id name (line, column)) [] = error $ "undefined type \"" ++ name ++ "\"; line " ++ show line ++ " column " ++ show column
lookup_type token@(Id name _) (t : rest) = case t of
  Record name2 _ | name == name2 -> t
  _ -> lookup_type token rest

subprogramtable_insert :: Subprogram -> (Int, Int) -> MemoryState -> MemoryState
subprogramtable_insert subprogram (line, col) st@(MemoryState _ _ _ subprogs _ _ _ _) =
  let name = get_subprogram_name subprogram
      already_defined = any (\sp -> get_subprogram_name sp == name) subprogs
  in if already_defined
    then error $ "subprogram \"" ++ name ++ "\" already defined in program; line " ++ show line ++ ", column " ++ show col
    else st { subprogramtable = subprogram : subprogs }

subprogramtable_update :: Subprogram -> MemoryState -> MemoryState
subprogramtable_update subprogram st@(MemoryState _ _ _ subprogs _ _ _ _) =
  let name = get_subprogram_name subprogram
      new_subprogs = subprogram : filter (\sp -> get_subprogram_name sp /= name) subprogs
  in st { subprogramtable = new_subprogs }

get_subprogram_name :: Subprogram -> String
get_subprogram_name subprogram@(Procedure name _ _) = name
get_subprogram_name subprogram@(Function name _ _ _) = name

lookup_function :: Token -> [Subprogram] -> Subprogram
lookup_function (Id name (line, column)) [] = error $ "undefined function \"" ++ name ++ "\" in program; line " ++ show line ++ " column " ++ show column
lookup_function token@(Id name (line, column)) (t : rest) = do
  let _ = traceShow "b" ();
  case t of
    Procedure proc_name _ _ | proc_name == name ->
      error $ "expected function, but \"" ++ name ++ "\" is a procedure; line " ++ show line ++ " column " ++ show column
    Function function_name _ _ _ | function_name == name -> t
    _ -> lookup_function token rest

lookup_procedure :: Token -> [Subprogram] -> Subprogram
lookup_procedure (Id name (line, column)) [] = error $ "undefined procedure \"" ++ name ++ "\" in program; line " ++ show line ++ " column " ++ show column
lookup_procedure token@(Id name (line, column)) (t : rest) = do
  let _ = traceShow "a" ();
  case t of
    Function function_name _ _ _ | function_name == name ->
      error $ "expected procedure, but \"" ++ name ++ "\" is a function; line " ++ show line ++ " column " ++ show column
    Procedure proc_name _ _ | proc_name == name -> t
    _ -> lookup_procedure token rest

typetable_insert :: Type -> (Int, Int) -> MemoryState -> MemoryState
typetable_insert t@(Record name fields) (line, column) st@(MemoryState _ _ table _ _ _ _ _) =
  let
    field_names = map fst fields
    duplicates = field_names \\ nub field_names
    existing_type_names = map get_type_name table
  in
    if name `elem` existing_type_names
      then error $ "type \"" ++ name ++ "\" already defined, alternative name must be provided; line " ++ show line ++ " column " ++ show column
    else if not (null duplicates)
      then error $ "duplicate field names in record " ++ name ++ ": " ++ show duplicates ++ "; line " ++ show line ++ " column " ++ show column
    else st {typetable = table ++ [t]}
typetable_insert _ _ st = st  -- ignora outros tipos

global_insert :: Symbol -> (Int, Int) -> MemoryState -> MemoryState
global_insert symbol@(name, _, _) (line, column) st =
    let declared_names = [n | (n, _, _) <- globals st] in
    if name `elem` declared_names
        then error $ "global variable \"" ++ name ++ "\" already declared; line: " ++ show line ++ ", column: " ++ show column
        else st { globals = globals st ++ [symbol] }

insert_symbol :: Symbol -> (Int, Int) -> MemoryState -> MemoryState
insert_symbol sym (line, column) st =
    case current_scope st of
        GlobalScope -> global_insert sym (line, column) st
        LocalScope -> local_insert sym (line, column) st

local_insert :: Symbol -> (Int, Int) -> MemoryState -> MemoryState
local_insert symbol@(name, _, _) (line, column) st =
  case call_stack st of
    [] -> error "internal error: no active scope to insert variable!"
    (ar : rest) ->
      case local_symbols ar of
        [] -> error "internal error: no local scopes found in current activation record!"
        scopes ->
          let
            in_local = any (elemName name) scopes
            in_global = any (\(n, _, _) -> n == name) (globals st)
          in
            if in_local || in_global
              then error $ "variable \"" ++ name ++ "\" already declared in scope (local or global); line: " ++ show line ++ ", column: " ++ show column
              else
                let (currentScope : otherScopes) = scopes
                    updatedScope = symbol : currentScope
                    updatedLocals = updatedScope : otherScopes
                    updatedAR = ar { local_symbols = updatedLocals }
                in st { call_stack = updatedAR : rest }
  where
    elemName :: String -> [Symbol] -> Bool
    elemName n = any (\(name', _, _) -> name' == n)

-- atualiza variável ou campo aninhado pela cadeia de acesso
symtable_update_by_access_chain :: [AccessStep] -> Type -> MemoryState -> MemoryState
symtable_update_by_access_chain [] _ st = st -- nada a fazer

-- caso final: só accessid = variável simples
symtable_update_by_access_chain [AccessId idTok@(Id name pos)] new_val st =
  symtable_update (idTok, new_val) st

-- caso geral: primeiro é accessid (variável base), resto é cadeia aninhada
symtable_update_by_access_chain (AccessId baseId@(Id base_name pos) : rest) new_val st =
  case call_stack st of
    [] -> error $ "internal error: no activation record found; cannot update variable " ++ base_name ++ "!"
    (ar : restStack) ->
      let localScopes = local_symbols ar
          foundLocal = findSymbolInScopes base_name localScopes
          updatedLocals = case foundLocal of
            Nothing -> localScopes
            Just _  -> updateSymbolInScopes base_name (updateNestedTypeInSymbol rest new_val pos) localScopes pos
      in case foundLocal of
          Just _ ->
            let updatedAR = ar { local_symbols = updatedLocals }
            in st { call_stack = updatedAR : restStack }
          Nothing ->
            case lookup base_name [(n, v) | (n, v, _) <- globals st] of
              Nothing -> error $ "undefined variable \"" ++ base_name ++ "\" in scope; line " ++ show (fst pos) ++ " column " ++ show (snd pos)
              Just baseType ->
                let updatedType = update_nested_type baseType rest new_val pos
                    updatedGlobals = updateEntry base_name updatedType (globals st) pos
                in st { globals = updatedGlobals }

symtable_update_by_access_chain _ _ st = st -- fallback seguro

-- atualiza variável simples (sem cadeia aninhada)
symtable_update :: (Token, Type) -> MemoryState -> MemoryState
symtable_update (Id id_name pos, new_value) st =
  case call_stack st of
    [] -> error $ "interal error: no activation record; variable \"" ++ id_name ++ "\" not found!"
    (ar : restStack) ->
      let localScopes = local_symbols ar
          foundLocal = findSymbolInScopes id_name localScopes
      in case foundLocal of
        Just _ ->
          let updatedLocals = updateSymbolInScopes id_name (const new_value) localScopes pos
              updatedAR = ar { local_symbols = updatedLocals }
          in st { call_stack = updatedAR : restStack }
        Nothing ->
          case lookup id_name [(n, t) | (n, t, _) <- globals st] of
            Nothing -> error $ "variable \"" ++ id_name ++ "\" not found in scope; line " ++ show (fst pos) ++ " column " ++ show (snd pos)
            Just _ ->
              let updatedGlobals = updateEntry id_name new_value (globals st) pos
              in st { globals = updatedGlobals }

symtable_update _ st = st

-- busca símbolo pelo nome nas listas de escopos (da mais interna para mais externa)
findSymbolInScopes :: String -> [[Symbol]] -> Maybe Type
findSymbolInScopes _ [] = Nothing
findSymbolInScopes name (scope:rest) =
  case lookup name [(n, t) | (n, t, _) <- scope] of
    Just typ -> Just typ
    Nothing  -> findSymbolInScopes name rest

-- atualiza tipo do símbolo na primeira ocorrência encontrada na lista de escopos
updateSymbolInScopes :: String -> (Type -> Type) -> [[Symbol]] -> (Int, Int) -> [[Symbol]]
updateSymbolInScopes _ _ [] _ = []
updateSymbolInScopes name f (scope:rest) (line, col) =
  if any (\(n, _, _) -> n == name) scope then map (\(n, t, isVar) ->
    if n == name
      then (n, f t, isVar)
      else (n, t, isVar)
    ) scope : rest
    else scope : updateSymbolInScopes name f rest (line, col)

-- atualiza entrada (global) com novo tipo para variável
updateEntry :: String -> Type -> [Symbol] -> (Int, Int) -> [Symbol]
updateEntry _ _ [] _ = []
updateEntry name newVal ((n, t, isVar) : rest) (line, col)
  | n == name = (n, newVal, isVar) : rest
  | otherwise = (n, t, isVar) : updateEntry name newVal rest (line, col)

-- atualiza tipo aninhado (recursivamente) pela cadeia de AccessStep
update_nested_type :: Type -> [AccessStep] -> Type -> (Int, Int) -> Type
-- atualiza campo de record
update_nested_type (Record name fields) (AccessField (Id field_name _) : rest) new_val pos =
  Record name $ map (\(fname, ftype) ->
    if fname == field_name then
      if null rest then (fname, new_val)
      else (fname, update_nested_type ftype rest new_val pos)
    else (fname, ftype)) fields

-- atualiza elemento de vetor
update_nested_type (Vector baseType elems) (AccessIndex idx : rest) new_val pos =
  if idx < 0 || idx >= length elems then
    error $ "index " ++ show idx ++ " out of bounds in vector; line " ++ show (fst pos) ++ " column " ++ show (snd pos)
  else if null rest then
    Vector baseType (replaceAt idx new_val elems)
  else
    Vector baseType (replaceAt idx (update_nested_type (elems !! idx) rest new_val pos) elems)

-- atualiza elemento de matriz
update_nested_type (Matrix baseType rows (row_count, col_count)) (AccessIndex2D i j : rest) new_val pos =
  if i < 0 || i >= length rows || j < 0 || j >= length (rows !! i) then
    error $ "index (" ++ show i ++ "," ++ show j ++ ") out of bounds in matrix; line " ++ show (fst pos) ++ " column " ++ show (snd pos)
  else if null rest then
    Matrix baseType (replaceAt i (replaceAt j new_val (rows !! i)) rows) (row_count, col_count)
  else
    let oldVal = (rows !! i) !! j
        updatedRow = replaceAt j (update_nested_type oldVal rest new_val pos) (rows !! i)
    in Matrix baseType (replaceAt i updatedRow rows) (row_count, col_count)

-- erro para acessos inválidos
update_nested_type t (step:_) _ (line, col) =
  error $ "cannot assign to " ++ show step ++ " in type " ++ show t ++ "; line " ++ show line ++ ", column " ++ show col

-- atualiza tipo de símbolo com cadeia AccessStep (wrapper para update_nested_type)
updateNestedTypeInSymbol :: [AccessStep] -> Type -> (Int, Int) -> Type -> Type
updateNestedTypeInSymbol steps newVal pos oldType = update_nested_type oldType steps newVal pos

-- substitui elemento na lista na posição idx
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx newVal xs =
  take idx xs ++ [newVal] ++ drop (idx + 1) xs

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
  (Vector t1 _, Vector t2 _) -> compare_type_base t1 t2
  (Matrix t1 _ (rows, cols), Matrix t2 _ (rows2, cols2)) -> compare_type_base t1 t2 && rows == rows2 && cols == cols2 
  _                          -> False

extract_base_type :: Type -> Type
extract_base_type t = case t of
  Integer _ -> Integer 0
  Floating _ -> Floating 0.0
  Str _ -> Str ""
  Boolean _ -> Boolean False
  Record name fields -> Record name (map (\(fname, ftype) -> (fname, extract_base_type ftype)) fields)
  Vector inner _ -> Vector (extract_base_type inner) []
  Matrix inner _ (rows, cols) ->
    let base = extract_base_type inner
        row = replicate cols base
        matrix = replicate rows row
    in Matrix base matrix (rows, cols)

is_type_vector :: Type -> Bool
is_type_vector (Vector _ _) = True
is_type_vector _            = False

is_type_matrix :: Type -> Bool
is_type_matrix (Matrix _ _ _) = True
is_type_matrix _            = False

measure_vector_size :: Type -> Int
measure_vector_size (Vector _ elems) = length elems
measure_vector_size _ = error "interal error: expected a vector type!"

get_type_name :: Type -> String
get_type_name (Record name _) = name
get_type_name _ = ""

get_id_name :: Token -> String
get_id_name (Id name _) = name
get_id_name _ = ""

extract_matrix_dimensions :: (Type, Type) -> (Int, Int) -> (Int, Int)
extract_matrix_dimensions (t1, t2) (line, col) = case (t1, t2) of
  (Integer 0, Integer x) -> error $ "invalid matrix size: both dimensions must be greater than zero, got 0" ++ "x" ++ show x ++ "; line: " ++ show line ++ "; column: " ++ show col
  (Integer x, Integer 0) -> error $ "invalid matrix size: both dimensions must be greater than zero, got " ++ show x ++ "x0; line: " ++ show line ++ "; column: " ++ show col
  (Integer x, Integer y) -> (x, y)
  _ -> error $ "matrix dimensions must be defined both as int values; line: " ++ show line ++ "; column: " ++ show col

--- funções para auxiliar na impressão de mensagens de erro

show_pretty_types :: Type -> String
show_pretty_types t = case t of
  Integer _ -> "int"
  Floating _ -> "float"
  Str _ -> "string"
  Boolean _ -> "bool"
  Record name _ -> name
  Matrix inner_type _ (rows, cols) -> "matrix<"++ show_pretty_types inner_type ++ 
    " : " ++ show rows ++ ", " ++ show cols ++ ">"
  Vector inner_type _ -> "vector<" ++ show_pretty_types inner_type ++ ">"

show_pretty_type_values :: Type -> String
show_pretty_type_values t = case t of
  Integer x -> "int (" ++ show x ++ ")"
  Floating x -> "float (" ++ show x ++ ")"
  Str x -> "string (" ++ show x ++ ")"
  Boolean x -> "bool (" ++ map toLower (show x) ++ ")"
  Record name fields -> name ++ " (" ++ show_fields fields ++ ")"
  Vector inner_type elements -> show_pretty_types t ++ " (" ++ type_to_string t ++ ")"
  Matrix inner_type elements _ -> show_pretty_types t ++ " (" ++ type_to_string t ++ ")"

-- função auxiliar para imprimir os campos formatados

show_fields :: [(String, Type)] -> String
show_fields [] = ""
show_fields [(fname, ftype)] = fname ++ ": " ++ show_pretty_type_values ftype
show_fields ((fname, ftype) : rest) = fname ++ ": " ++ show_pretty_type_values ftype ++ ", " ++ show_fields rest

error_not_found :: String -> Int -> Int -> a
error_not_found name line column =
  error $ "undefined variable \"" ++ name ++ "\" in current scope; line " ++ show line ++ " column " ++ show column

binary_type_error :: String -> Type -> Type -> (Int, Int) -> a
binary_type_error op_name first_type second_type (line, column) =
  error $ "type error in \"" ++ op_name ++ "\": unexpected parameter types " ++
  show_pretty_type_values first_type ++ " and " ++ show_pretty_type_values second_type ++
  "; line: " ++ show line ++ ", column: " ++ show column

variable_type_error_msg :: String -> Type -> Type -> (Int, Int) -> String
variable_type_error_msg variable_name expected_type provided_type (line, column) = "type error: name " ++ show variable_name
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
const_guess_declaration_assignment_error_msg (line, column) = "constants are of no bindability to user-defined types or data-structures; line: " ++ show line ++ ", column: " ++ show column

print_types :: ParsecT [Token] MemoryState IO ()
print_types = do
  st <- getState
  liftIO $ putStrLn ("user defined types: " ++ show (typetable st))

print_subprograms :: ParsecT [Token] MemoryState IO ()
print_subprograms = do
  st <- getState
  liftIO $ putStrLn ("available subprograms: " ++ show (subprogramtable st))

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
parser = runParserT program (MemoryState { globals = [], call_stack = [] , typetable = [], subprogramtable = [], executing = False, in_procedure = False, current_scope = GlobalScope, last_parsed_function = Nothing }) "Parsing error!"

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "uso: seu_programa <nome_do_arquivo>"
    else do
      let filename = head args
      tokens <- getTokens filename
      result <- parser tokens
      case result of
        Left err -> print err
        Right ans -> return ()
