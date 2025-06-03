{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$char = [^"\\]

tokens :-

  $white+                              ;
  "//".*                               ;
  procedure                            { \s -> Procedure }
  function                             { \s -> Function }
  ";"                                  { \s -> SemiColon}
  ":"                                  { \s -> Colon}
  ","                                  { \s -> Comma}
  "."                                  { \s -> Dot}
  struct                               { \s -> Struct }
  enum                                 { \s -> Enum }
  const                                { \s -> Const }
  "{"                                  { \s -> BracketLeft }
  "}"                                  { \s -> BracketRight }
  "["                                  { \s -> BraceLeft }
  "]"                                  { \s -> BraceRight }
  "("                                  { \s -> ParenLeft }
  ")"                                  { \s -> ParenRight }
  ==                                   { \s -> Equal }
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
  \_? [$alpha] [$alpha $digit \_]*         { \s -> Id s }
  \"[$char \\.]*\"                     { \s -> String (read s)}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
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
  If  |
  Else  |
  Match  |
  Case  |
  Default  |
  For  |
  While  |
  Repeat  |
  Until  |
  Equal |
  Greater |
  GreaterEq |
  Less |
  LessEq |
  Struct |
  Enum |
  Type String |
  Id String |
  Float Float |
  Int Int |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
