{
module Lexer where
}

%wrapper "monad"

tokens :-
  $white+ ;
  :ID          { \s -> IdToken }
  :LABEL       { \s -> LabelToken }
  :START_ID    { \s -> StartIdToken }
  :END_ID      { \s -> EndIdToken }
  :TYPE        { \s -> TypeToken }
  string       { \s -> StringToken s }
  integer      { \s -> IntegerToken (read s) }
  boolean      { \s -> BooleanToken (s == "true") }
  null         { \s -> NullToken }
  ','          { \s -> CommaToken }
  ';'          { \s -> SemicolonToken }
  '('          { \s -> LeftParenToken }
  ')'          { \s -> RightParenToken }
  '"'          { \s -> DoubleQuoteToken }
  "imported"   { \s -> ImportedToken }