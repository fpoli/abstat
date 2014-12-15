module While.Parser (
    whileParser,
    parseString,
    parseFile,
    parseAExpression,
    parseBExpression
) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import While.Lexer
import While.AST

-- Main parser

whileParser :: Parser Stmt
whileParser =  whiteSpace >> (
                    (eof >> return Skip)
                <|> do
                    prog <- statementSeq
                    eof
                    return prog
                )

statementSeq :: Parser Stmt
statementSeq =
    try ( do -- maybe it's the last statement of the program/block
        s <- statement
        semi
        others <- statementSeq
        return (Seq s others)
    ) <|> do
        s <- statement
        optional semi
        return s

statement :: Parser Stmt
statement =  parens statementSeq
         <|> ifStmt
         <|> whileStmt
         <|> skipStmt
         <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
    do  reserved "if"
        cond  <- bExpression
        reserved "then"
        stmt1 <- statement
        optional semi
        reserved "else"
        stmt2 <- statement
        return $ If cond stmt1 stmt2
 
whileStmt :: Parser Stmt
whileStmt =
    do  reserved "while"
        cond <- bExpression
        reserved "do"
        stmt <- statement
        return $ While cond stmt
 
assignStmt :: Parser Stmt
assignStmt =
    do  var  <- identifier
        reservedOp ":="
        expr <- aExpression
        return $ Assign var expr
 
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

-- Expressions

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm
 
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators :: OperatorTable Char () AExpr
aOperators = [
        [Prefix (reservedOp "-"   >> return (AUnary Neg      ))          ],
        [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft],
        [Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft],
        [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft],
        [Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
    ]

bOperators :: OperatorTable Char () BExpr
bOperators = [
        [Prefix (reservedOp "not" >> return (BUnary Not      ))          ],
        [Prefix (reservedOp "!"   >> return (BUnary Not      ))          ],
        [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft],
        [Infix  (reservedOp "&&"  >> return (BBinary And     )) AssocLeft],
        [Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft],
        [Infix  (reservedOp "||"  >> return (BBinary Or      )) AssocLeft]
    ]

aTerm :: Parser AExpr
aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm :: Parser BExpr
bTerm =  try (parens bExpression) -- maybe is an arithmetic parentesis
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression :: Parser BExpr
rExpression =
    do  a1 <- aExpression
        op <- relation
        a2 <- aExpression
        return $ RBinary op a1 a2

relation :: Parser RBinOp
relation =  (reservedOp ">"  >> return Greater     )
        <|> (reservedOp ">=" >> return GreaterEqual)
        <|> (reservedOp "<"  >> return Less        )
        <|> (reservedOp "<=" >> return LessEqual   )
        <|> (reservedOp "="  >> return Equal       )
        <|> (reservedOp "==" >> return Equal       )
        <|> (reservedOp "!=" >> return NotEqual    )
        <|> (reservedOp "<>" >> return NotEqual    )

-- Handy functions

parseAExpression :: String -> AExpr
parseAExpression str =
    case parse aExpression "" str of
        Left e  -> error $ show e
        Right r -> r

parseBExpression :: String -> BExpr
parseBExpression str =
    case parse bExpression "" str of
        Left e  -> error $ show e
        Right r -> r

parseString :: String -> Stmt
parseString str =
    case parse whileParser "" str of
        Left e  -> error $ show e
        Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
    do  program  <- readFile file
        case parse whileParser "" program of
            Left e  -> print e >> fail "parse error"
            Right r -> return r
