-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need
import Text.ParserCombinators.Parsec
import Data.Char

--type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString s = parse startParse "" s

-- Zero or more whitespace
whitespace :: Parser ()
whitespace = do { many $ oneOf " \t"; return () }
-- One or more whitespace
whitespace1 :: Parser ()
whitespace1 = do { many1 $ oneOf " \t"; return () }

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

reservedKeywords :: [String]
reservedKeywords = ["None", "True", "False", "for", "if", "in", "not"]

--identHead :: Parser Char
--identHead = letter <|> char '_'
--identTail :: Parser Char
--identTail = alphaNum <|> char '_'

ident :: Parser String
ident = do
    c <- identHead
    cs <- many identTail
    let s = (c:cs)
    if s `elem` reservedKeywords then
      unexpected $ "reserved keyword '" ++ s ++ "'"
    else return s
  where
    identHead = satisfy (\c -> isLetter c || c == '_')
    identTail = satisfy (\c -> isDigit c || isLetter c || c == '_')

numConst :: Parser Int
numConst = do
  c <- satisfy (\char -> (char > '0' && char <= '9') || char == '-')
  s <- many digit
  if c == '-' && (length s) == 0 then unexpected "-"
  else return $ read (c:s)

stringDelim :: Parser Char
stringDelim = satisfy (\char -> char == '\'')

escapeCodes :: Parser Char
escapeCodes = char '\\' >> ((char 'n' >> return '\n') <|> oneOf ("\\\'"))

nonEscapeCodes :: Parser Char
nonEscapeCodes = noneOf "\\\'\n"

stringBody :: Parser Char
stringBody = nonEscapeCodes <|> escapeCodes

stringConst :: Parser String
stringConst = do
  s <- between stringDelim stringDelim (many stringBody)
  return s

constInt :: Parser Exp
constInt = do
  n <- lexeme numConst
  return $ Const (IntVal n)

constString :: Parser Exp
constString = do
  s <- lexeme stringConst
  return $ Const (StringVal s)

var :: Parser Exp
var = do
  vname <- lexeme ident
  return $ Var vname

kwExp :: Parser Exp
kwExp =
  do { string "None"; return $ Const NoneVal }
  <|> do { string "True"; return $ Const TrueVal }
  <|> do { string "False"; return $ Const FalseVal }


operators = ["+","-","*","//","%",
             "==","!=","<","<=",">",">=",
             "in", "not in"]

operation :: Parser Exp
operation = do
  e1 <- lexeme expression
  spaces
  op <- string "+"
  return (Oper Plus e1 e1)

--opExp :: Parser Exp
--opExp = do
--  e1 <- lexeme expression

expression :: Parser Exp
expression =
          constInt
          <|> constString
          <|> kwExp
          <|> var
          -- <|> operation

definitionStmt :: Parser Stmt
definitionStmt = do
  vname <- lexeme ident
  lexeme $ satisfy (\char -> char == '=')
  exp <- lexeme expression
  return $ SDef vname exp

expressionStmt :: Parser Stmt
expressionStmt = do
  exp <- lexeme expression
  return $ SExp exp

statement :: Parser Stmt
statement = do
  stmt <- try definitionStmt <|> expressionStmt
  spaces
  --whitespace
  --newline
  return stmt

startParse :: Parser Program
startParse = do { spaces ; p <- many1 statement; spaces; eof; return p }
--startParse = do { spaces; p <- statement `sepBy1` newline ; eof; return p }