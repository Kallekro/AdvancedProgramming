module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E  ::= T E' | "-" T E' .
--   E' ::= "+" T E' | "-" T E' | ()
--   T  ::= num | "(" E ")" .
-- Further specification:
--   () is empty

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

digit :: Parser Char    -- for portability to Parsec
digit = satisfy (\char -> char >= '0' && char <= '9')

whitespace :: Parser () -- for portability to Parsec
whitespace = skipSpaces

parseString :: String -> Either ParseError Exp
parseString s =
  if length s == 0 then Left "Nothing to parse."
  else
    let parser = do whitespace
                    res <- eNT
                    return res
    in case readP_to_S parser s of
    (x:xs) -> case last (x:xs) of
                (exp, "") -> Right exp
                (_, rem) -> Left $ "Could not parse remainder: " ++ rem
    [] -> Left "Could not parse."

-- From this point the code is identical to WarmupParsec.hs
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
eOptNT e = do e2 <- add e   -- "+" T E'
              eOptNT e2
       <|> do e2 <- minus e -- "-" T E'
              eOptNT e2
       <|> return e         -- ()

tNT :: Parser Exp
tNT = number                -- num
  <|> between lPar rPar eNT -- "(" E ")"
