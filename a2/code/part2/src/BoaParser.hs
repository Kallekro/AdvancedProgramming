-- Skeleton file for Boa Parser.
module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need
import Text.ParserCombinators.Parsec
import Data.Char

--type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString s = parse startParse "" s

-- Zero or more whitespace (without newline)
whitespace :: Parser ()
whitespace = do { many $ oneOf " \t"; return () }
-- One or more whitespace (without newline)
whitespace1 :: Parser ()
whitespace1 = do { many1 $ oneOf " \t"; return () }

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

reservedKeywords :: [String]
reservedKeywords = ["None", "True", "False", "for", "if", "in", "not"]

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

-- TODO: Right now \\ is parsed as \\ but should be parsed as \
-- unintuitively \' is correctly parsed as '
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

constExp :: Parser Exp
constExp = constInt <|> constString

varExp :: Parser Exp
varExp = do
  vname <- lexeme ident
  return $ Var vname

kwExp :: Parser Exp
kwExp =
  do { string "None"; return $ Const NoneVal }
  <|> do { string "True"; return $ Const TrueVal }
  <|> do { string "False"; return $ Const FalseVal }

-- In the following array the order matters,
-- in particular '<=' and '>=' must come before '<' and '>'
-- since we're matching from left to right in the list.
operators :: [String]
operators = ["+", "-", "*", "//", "%",
             "==", "!=", "<=", ">=",
             "<", ">", "in", "not in"]

matchOperator :: [String] -> Parser String
matchOperator ops =
  case ops of
    (x:xs) -> try $ string x <|> matchOperator xs
    _ -> unexpected "unknown operator"

-- TODO: Look at reducing size, maybe by storing each operator string
-- with their respective operation of type Op and using this.
-- There is probably gonna be a challenge getting the Not'ed operators to work.
operation :: Exp -> Parser Exp
operation e1 = do
  op <- lexeme $ matchOperator operators
  e2 <- lexeme tNT
  case op of
    "+" -> return $ Oper Plus e1 e2
    "-" -> return $ Oper Minus e1 e2
    "*" -> return $ Oper Times e1 e2
    "//" -> return $ Oper Div e1 e2
    "%" -> return $ Oper Mod e1 e2
    "==" -> return $ Oper Eq e1 e2
    "!=" -> return $ Not $ Oper Eq e1 e2
    "<" -> return $ Oper Less e1 e2
    "<=" -> return $ Not $ Oper Greater e1 e2
    ">" -> return $ Oper Greater e1 e2
    ">=" -> return $ Not $ Oper Less e1 e2
    "in" -> return $ Oper In e1 e2
    "not in" -> return $ Not $ Oper In e1 e2
    _ -> unexpected (show op)

notExp :: Parser Exp
notExp = do
  lexeme $ string "not"
  e1 <- lexeme expression
  return $ Not e1
     
built_ins :: [String]
built_ins = ["range", "print"]

lPar :: Parser Char
lPar = lexeme $ satisfy (\char -> char == '(')
rPar :: Parser Char
rPar = lexeme $ satisfy (\char -> char == ')')

callFun :: Parser Exp
callFun = do
  fname <- lexeme $ ident
  if not (fname `elem` built_ins) then
    unexpected $ "unknown function '" ++ fname ++ "'"
  else do
    lPar
    expList <- tNT `sepBy` (lexeme $ char ',')
    rPar
    return $ Call fname expList

expression :: Parser Exp
expression = do e <- tNT
                expressionOpt e

expressionOpt :: Exp -> Parser Exp
expressionOpt e1 = (do { e2 <- try $ operation e1; expressionOpt e2 })
                  <|> return e1

tNT :: Parser Exp
tNT = constExp
  <|> notExp
  <|> kwExp
  <|> (try callFun <|> varExp)
  <|> between lPar rPar expression

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
  (statementSep <|> eof)
  return stmt

statementSep :: Parser ()
statementSep = do
  whitespace
  satisfy (\char -> char == '\n' || char == ';')
  spaces

startParse :: Parser Program
startParse = do { spaces ; p <- many1 statement; spaces; eof; return p }