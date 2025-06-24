{
module Lexer where

import System.IO
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$char = [^"\\]

tokens :-

  $white+                              ;
  "//".*                               ;
  main                                 { \s -> Main }
  procedure                            { \s -> Procedure }
  function                             { \s -> Function }
  ";"                                  { \s -> SemiColon}
  ":"                                  { \s -> Colon}
  ","                                  { \s -> Comma}
  "."                                  { \s -> Dot}
  struct                               { \s -> Struct }
  enum                                 { \s -> Enum }
  print                                { \s -> Print }
  scan                                 { \s -> Scan }
  const                                { \s -> Const }
  guess                                { \s -> Guess }
  int                                  { \s -> Type s }
  float                                { \s -> Type s }
  bool                                 { \s -> Type s }
  string                               { \s -> Type s }
  vector                               { \s -> Vector }
  matrix                               { \s -> Matrix }
  true                                 { \s -> Bool True }
  false                                { \s -> Bool False }
  "{"                                  { \s -> BracketLeft }
  "}"                                  { \s -> BracketRight }
  "["                                  { \s -> BraceLeft }
  "]"                                  { \s -> BraceRight }
  "("                                  { \s -> ParenLeft }
  ")"                                  { \s -> ParenRight }
  ==                                   { \s -> Equal }
  "!="                                 { \s -> NotEqual}
  >                                    { \s -> Greater}
  >=                                   { \s -> GreaterEq}
  "<"                                  { \s -> Less}
  "<="                                 { \s -> LessEq}
  "+="                                 { \s -> AddAssign}
  "-="                                 { \s -> SubAssign}
  "*="                                 { \s -> MulAssign}
  "/="                                 { \s -> DivAssign}
  "%="                                 { \s -> RemAssign}
  "^="                                 { \s -> PowAssign}
  "and" | "&&"                         { \s -> And}
  "or" | "||"                          { \s -> Or}
  "not" | "!"                          { \s -> Not}
  "+"                                  { \s -> Add}
  "-"                                  { \s -> Sub}
  "*"                                  { \s -> Mul}
  "/"                                  { \s -> Div}
  "%"                                  { \s -> Rem}
  "^"                                  { \s -> Pow}
  =                                    { \s -> Assign}
  return                               { \s -> Return}
  break                                { \s -> Break}
  continue                             { \s -> Continue}
  leave                                { \s -> Leave}
  if                                   { \s -> If}
  else                                 { \s -> Else}
  while                                { \s -> While}
  for                                  { \s -> For}
  repeat                               { \s -> Repeat}
  until                                { \s -> Until}
  match                                { \s -> Match}
  case                                 { \s -> Case}
  default                              { \s -> Default}
  $digit+\.$digit+                     { \s -> Float (read s) }
  $digit+                              { \s -> Int (read s) }
  \_? [$alpha] [$alpha $digit \_]*     { \s -> Id s }
  \"[$char \\.]*\"                     { \s -> String (read s)}
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Main    |
  Procedure |
  Function |
  Comma   |
  Dot   |
  SemiColon |
  Colon |
  Const |
  Assign    |
  AddAssign |
  SubAssign |
  MulAssign |
  DivAssign |
  RemAssign |
  PowAssign |
  Vector  |
  Matrix  |
  Print   |
  Scan    |
  Add    |
  Sub    |
  Mul    |
  Div    |
  Rem    |
  Pow    |
  And    |
  Or    |
  Not    |
  BracketLeft    |
  BracketRight    |
  BraceLeft    |
  BraceRight    |
  ParenLeft    |
  ParenRight    |
  Return  |
  Break   |
  Continue |
  Leave   |
  If  |
  Else  |
  Match  |
  Case  |
  Default  |
  For  |
  While  |
  Repeat  |
  Until  |
  NotEqual |
  Equal |
  Greater |
  GreaterEq |
  Less |
  LessEq |
  Struct |
  Guess |
  Enum |
  Type String |
  Id String |
  Float Float |
  Int Int |
  Bool Bool |
  String String 
  deriving (Eq,Show)

getTokens fn = do
    fh <- openFile fn ReadMode;
    s <- hGetContents fh;
    return (alexScanTokens s)
}
