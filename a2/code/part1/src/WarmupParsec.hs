module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   <<< fill in here, if different from WarmupReadP >>>
-- We used the same grammar as with ReadP

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s =
  let parser = do whitespace
                  res <- eNT
                  eof
                  return res
  in parse parser "" s

whitespace :: Parser ()
whitespace = do { many $ oneOf " \n\t"; return () }

-- From this point the code is identical to WarmupReadP.hs
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

number :: Parser Exp
number = do
  n <- lexeme $ many1 digit
  return $ Num (read n)

add :: Exp -> Parser Exp
add e1 = do
  lexeme $ satisfy (\char -> char == '+')
  exp2 <- tNT
  return $ Add e1 exp2

neg :: Parser Exp
neg = do
  lexeme $ satisfy (\char -> char == '-')
  exp1 <- tNT
  return $ Negate exp1

minus :: Exp -> Parser Exp
minus e1 = do
  lexeme $ satisfy (\char -> char == '-')
  exp2 <- tNT
  return $ Add e1 (Negate exp2)

lPar :: Parser Char
lPar = lexeme $ satisfy (\char -> char == '(')
rPar :: Parser Char
rPar = lexeme $ satisfy (\char -> char == ')')

eNT :: Parser Exp
eNT = do {e <- tNT; eOptNT e} -- T E'
  <|> do {e <- neg; eOptNT e} -- "-" T E'

eOptNT :: Exp -> Parser Exp
eOptNT e =  do e2 <- add e   -- "+" T E'
               eOptNT e2
        <|> do e2 <- minus e -- "-" T E'
               eOptNT e2
        <|> return e         -- ()

tNT :: Parser Exp
tNT = number                -- num
  <|> between lPar rPar eNT -- "(" E ")"
