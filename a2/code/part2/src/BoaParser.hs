module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need
import Text.ParserCombinators.Parsec
import Data.Char

parseString :: String -> Either ParseError Program
parseString s = parse startParse "" s

-- Zero or more whitespace (without newline)
whitespace :: Parser ()
whitespace = try comment <|> do { many $ oneOf " \t"; return () }
-- One or more whitespace (without newline)
whitespace1 :: Parser ()
whitespace1 = try comment <|> do { many1 $ oneOf " \t"; return () }

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  spaces
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
numConst =
  (do
    c <- satisfy (\char -> (char > '0' && char <= '9') || char == '-')
    s <- many digit
    if c == '-' && (length s) == 0 then unexpected "-"
    else return $ read (c:s))
  <|> do {zero <- satisfy (\char -> char == '0'); return $ read [zero]}

stringDelim :: Parser Char
stringDelim = satisfy (\char -> char == '\'')

escapeCodes :: Parser Char
escapeCodes = char '\\' >> (
  (char 'n' >> return '\n') <|>
  oneOf ("\\\'\n"))

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
  do { lexeme $ string "None"; return $ Const NoneVal }
  <|> do { lexeme $ string "True"; return $ Const TrueVal }
  <|> do { lexeme $ string "False"; return $ Const FalseVal }

matchOperator :: [String] -> Parser String
matchOperator ops =
  case ops of
    (x:xs) -> try (string x) <|> matchOperator xs
    [] -> try (do {string "not"; whitespace1; string "in"; return "not in"})
          <|> unexpected "unknown operator"

multiplicative :: Exp -> Bool -> Parser Exp
multiplicative e1 parFlag = do
  op2 <- lexeme $ matchOperator ["*", "//", "%"]
  (e2,_) <- lexeme tNT
  case (e1, parFlag) of
    (Oper op1 e1a e1b, False) ->
      case op2 of
        "*" -> return $ Oper op1 e1a (Oper Times e1b e2)
        "//" -> return $ Oper op1 e1a (Oper Div e1b e2)
        "%" -> return $ Oper op1 e1a (Oper Mod e1b e2)
        _ -> unexpected (show op2)
    _ ->
      case op2 of
        "*" -> return $ Oper Times e1 e2
        "//" -> return $ Oper Div e1 e2
        "%" -> return $ Oper Mod e1 e2
        _ -> unexpected (show op2)

additive :: Exp -> Parser Exp
additive e1 = do
  op <- lexeme $ matchOperator ["+", "-"]
  (e2,_) <- lexeme tNT
  case op of
    "+" -> return $ Oper Plus e1 e2
    "-" -> return $ Oper Minus e1 e2
    _ -> unexpected (show op)

relational :: Exp -> Parser Exp
relational e1 = do
  op <- lexeme $ matchOperator [ "==", "!=", "<=", ">=",
                                 "<", ">", "in", "not in"]
  (e2,_) <- lexeme tNT
  case op of
    "==" -> return $ Oper Eq e1 e2
    "!=" -> return $ Not $ Oper Eq e1 e2
    "<=" -> return $ Not $ Oper Greater e1 e2
    ">=" -> return $ Not $ Oper Less e1 e2
    "<" -> return $ Oper Less e1 e2
    ">" -> return $ Oper Greater e1 e2
    "in" -> return $ Oper In e1 e2
    "not in" -> return $ Not $ Oper In e1 e2
    _ -> unexpected (show op)

operationNoRelational :: Exp -> Bool -> Parser Exp
operationNoRelational e1 parFlag = try (multiplicative e1 parFlag)
                                   <|> additive e1

operationWithRelational :: Exp -> Bool -> Parser (Exp, Bool)
operationWithRelational e1 parFlag =
  try (do {relOp <- relational e1; return (relOp, True)})
  <|> (do {nonRelOp <- operationNoRelational e1 parFlag;
           return (nonRelOp, False) })

someAfterWhitespaceOrEnclosed :: Parser a -> Parser a
someAfterWhitespaceOrEnclosed p  =
  try (do
    whitespace1
    e <- p
    return e)
  <|> do
    whitespace
    e <- between lPar rPar p
    return e

expAfterWhitespaceOrEnclosed :: Parser Exp
expAfterWhitespaceOrEnclosed =
  someAfterWhitespaceOrEnclosed expression
  <|> do
    e <- listExp
    return e

notExp :: Parser Exp
notExp = string "not" >> do
  e <- lexeme $ expAfterWhitespaceOrEnclosed
  return $ Not e

lBracket :: Parser Char
lBracket = lexeme $ satisfy (\char -> char == '[')
rBracket :: Parser Char
rBracket = lexeme $ satisfy (\char -> char == ']')

expList :: Parser [Exp]
expList = expression `sepBy` (lexeme $ char ',')

listExp :: Parser Exp
listExp = do
  lBracket
  es <- expList
  rBracket
  return $ List es

qualFor :: Parser Qual
qualFor = do
  string "for"
  vname <- lexeme $ someAfterWhitespaceOrEnclosed ident
  string "in"
  e <- lexeme $ expAfterWhitespaceOrEnclosed
  return $ QFor vname e

qualIf :: Parser Qual
qualIf = do
  string "if"
  e <- lexeme expAfterWhitespaceOrEnclosed
  return $ QIf e

qualAny :: Parser Qual
qualAny = qualFor <|> qualIf

listComprExp :: Parser Exp
listComprExp = do
  lBracket
  e <- lexeme expression
  q1 <- lexeme qualFor
  qs <- many $ lexeme qualAny
  rBracket
  return $ Compr e (q1:qs)

lPar :: Parser Char
lPar = lexeme $ satisfy (\char -> char == '(')
rPar :: Parser Char
rPar = lexeme $ satisfy (\char -> char == ')')

callFun :: Parser Exp
callFun = do
  fname <- lexeme $ ident
  lPar
  es <- expList
  rPar
  return $ Call fname es

expression :: Parser Exp
expression = do (e, parFlag) <- tNT
                expressionOpt1 e parFlag

expressionOpt1 :: Exp -> Bool -> Parser Exp
expressionOpt1 e1 parFlag = (
  do (e2, flag) <- try (operationWithRelational e1 parFlag)
     if flag then expressionOpt2 e2 parFlag
     else expressionOpt1 e2 parFlag)
  <|> return e1

expressionOpt2 :: Exp -> Bool -> Parser Exp
expressionOpt2 e1 parFlag =
  (do e2 <- try $ operationNoRelational e1 parFlag
      expressionOpt2 e2 parFlag)
  <|> return e1

tNT :: Parser (Exp, Bool)
tNT = do {e <- constExp; return (e, False)}
  <|> do {e <- try notExp; return (e, False)}
  <|> do {e <- try kwExp; return (e, False)}
  <|> do {e <- (try listExp <|> listComprExp); return (e, False)}
  <|> do {e <- (try callFun <|> varExp); return (e, False)}
  <|> do {e <- between lPar rPar expression; return (e, True)}

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

comment :: Parser ()
comment = do
  satisfy (\char -> char == '#')
  many $ noneOf "\n"
  (newline >> return ()) <|> eof
  try (spaces >> eof) <|> spaces

statement :: Parser Stmt
statement = do
  stmt <- try definitionStmt <|> expressionStmt
  statementSep <|> comment <|> eof
  many comment
  return stmt

statementSep :: Parser ()
statementSep = do
  whitespace
  satisfy (\char ->char == ';')
  spaces

startParse :: Parser Program
startParse = do
  spaces
  many comment
  p <- many1 statement
  spaces >> eof >> return p