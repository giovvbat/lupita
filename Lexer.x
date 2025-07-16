{
module Lexer where

import System.IO
import Text.Read (readMaybe)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$char = [^"\\]

tokens :-

  $white+                              ;
  "//".*                               ;
  main                                 { \p s -> Main (getLC p) }
  procedure                            { \p s -> Proc (getLC p) }
  function                             { \p s -> Func (getLC p) }
  ";"                                  { \p s -> SemiColon (getLC p) }
  ":"                                  { \p s -> Colon (getLC p) }
  ","                                  { \p s -> Comma (getLC p) }
  "."                                  { \p s -> Dot (getLC p) }
  struct                               { \p s -> Struct (getLC p) }
  print                                { \p s -> Print (getLC p) }
  scan                                 { \p s -> Scan (getLC p) }
  remove                               { \p s -> Remove (getLC p) }
  push                                 { \p s -> Push (getLC p) }
  count_rows                           { \p s -> CountRows (getLC p) }
  count_columns                        { \p s -> CountCols (getLC p) }
  length                               { \p s -> Length (getLC p) }
  const                                { \p s -> Const (getLC p) }
  guess                                { \p s -> Guess (getLC p) }
  int                                  { \p s -> Type s (getLC p) }
  float                                { \p s -> Type s (getLC p) }
  bool                                 { \p s -> Type s (getLC p) }
  string                               { \p s -> Type s (getLC p) }
  vector                               { \p s -> Vectr (getLC p) }
  matrix                               { \p s -> Matrx (getLC p) }
  true                                 { \p s -> Bool True (getLC p) }
  false                                { \p s -> Bool False (getLC p) }
  "&"                                  { \p s -> Reference (getLC p) }
  "{"                                  { \p s -> BracketLeft (getLC p) }
  "}"                                  { \p s -> BracketRight (getLC p) }
  "["                                  { \p s -> BraceLeft (getLC p) }
  "]"                                  { \p s -> BraceRight (getLC p) }
  "("                                  { \p s -> ParenLeft (getLC p) }
  ")"                                  { \p s -> ParenRight (getLC p) }
  ==                                   { \p s -> Equal (getLC p) }
  "!="                                 { \p s -> NotEqual (getLC p) }
  >                                    { \p s -> Greater (getLC p) }
  >=                                   { \p s -> GreaterEq (getLC p) }
  "<"                                  { \p s -> Less (getLC p) }
  "<="                                 { \p s -> LessEq (getLC p) }
  "+="                                 { \p s -> AddAssign (getLC p) }
  "-="                                 { \p s -> SubAssign (getLC p) }
  "*="                                 { \p s -> MulAssign (getLC p) }
  "/="                                 { \p s -> DivAssign (getLC p) }
  "%="                                 { \p s -> RemAssign (getLC p) }
  "^="                                 { \p s -> PowAssign (getLC p) }
  "and" | "&&"                         { \p s -> And (getLC p) }
  "or" | "||"                          { \p s -> Or (getLC p) }
  "not" | "!"                          { \p s -> Not (getLC p) }
  "+"                                  { \p s -> Add (getLC p) }
  "-"                                  { \p s -> Sub (getLC p) }
  "*"                                  { \p s -> Mul (getLC p) }
  "/"                                  { \p s -> Div (getLC p) }
  "%"                                  { \p s -> Rem (getLC p) }
  "^"                                  { \p s -> Pow (getLC p) }
  =                                    { \p s -> Assign (getLC p) }
  return                               { \p s -> Return (getLC p) }
  break                                { \p s -> Break (getLC p) }
  continue                             { \p s -> Continue (getLC p) }
  leave                                { \p s -> Leave (getLC p) }
  if                                   { \p s -> If (getLC p) }
  else                                 { \p s -> Else (getLC p) }
  while                                { \p s -> While (getLC p) }
  for                                  { \p s -> For (getLC p) }
  repeat                               { \p s -> Repeat (getLC p) }
  until                                { \p s -> Until (getLC p) }
  match                                { \p s -> Match (getLC p) }
  case                                 { \p s -> Case (getLC p) }
  default                              { \p s -> Default (getLC p) }
  $digit+\.$digit+                     { \p s -> Float (read s) (getLC p) }
  $digit+                              { \p s -> Int (read s) (getLC p) }
  \_? [$alpha] [$alpha $digit \_]*     { \p s -> Id s (getLC p) }
  \"($char|\\.)*\"                     { \p s -> String (read s) (getLC p) }
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Main (Int, Int) |
  Proc (Int, Int) |
  Func (Int, Int) |
  Comma   (Int, Int) |
  Dot   (Int, Int) |
  SemiColon (Int, Int) |
  Colon (Int, Int) |
  Const (Int, Int) |
  Assign    (Int, Int) |
  AddAssign (Int, Int) |
  SubAssign (Int, Int) |
  MulAssign (Int, Int) |
  DivAssign (Int, Int) |
  RemAssign (Int, Int) |
  PowAssign (Int, Int) |
  Vectr  (Int, Int) |
  Matrx  (Int, Int) |
  Remove (Int, Int) |
  Push (Int, Int) |
  CountRows (Int, Int) |
  CountCols (Int, Int) |
  Length (Int, Int) |
  Print   (Int, Int) |
  Scan    (Int, Int) |
  Add    (Int, Int) |
  Sub    (Int, Int) |
  Mul    (Int, Int) |
  Div    (Int, Int) |
  Rem    (Int, Int) |
  Pow    (Int, Int) |
  And    (Int, Int) |
  Or    (Int, Int) |
  Not    (Int, Int) |
  Reference (Int, Int) |
  BracketLeft   (Int, Int) |
  BracketRight    (Int, Int) |
  BraceLeft    (Int, Int) |
  BraceRight    (Int, Int) |
  ParenLeft    (Int, Int) |
  ParenRight    (Int, Int) |
  Return  (Int, Int) |
  Break   (Int, Int) |
  Continue (Int, Int) |
  Leave   (Int, Int) |
  If  (Int, Int) |
  Else  (Int, Int) |
  Match  (Int, Int) |
  Case  (Int, Int) |
  Default  (Int, Int) |
  For  (Int, Int) |
  While  (Int, Int) |
  Repeat  (Int, Int) |
  Until  (Int, Int) |
  NotEqual (Int, Int) |
  Equal (Int, Int) |
  Greater (Int, Int) |
  GreaterEq (Int, Int) |
  Less (Int, Int) |
  LessEq (Int, Int) |
  Struct (Int, Int) |
  Guess (Int, Int) |
  Type String (Int, Int) |
  Id String (Int, Int) |
  Float Float (Int, Int) |
  Int Int (Int, Int) |
  Bool Bool (Int, Int) |
  String String (Int, Int)
  deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)

getTokens fn = do
    fh <- openFile fn ReadMode;
    s <- hGetContents fh;
    return (alexScanTokens s)
}
