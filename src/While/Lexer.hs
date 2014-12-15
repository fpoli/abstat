module While.Lexer where

import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: LanguageDef st
languageDef = emptyDef {
        Token.commentStart = "/*",
        Token.commentEnd = "*/",
        Token.commentLine = "//",
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = [
                "true",
                "false",
                "skip",
                "if",
                "then",
                "else",
                "while",
                "do"
            ],
        Token.reservedOpNames = [
                "+",
                "-",
                "*",
                "/",
                "<",
                "<=",
                ">",
                ">=",
                "=", "==",
                "<>", "!=",
                "and", "&&",
                "or", "||",
                "not", "!"
           ]
    }

lexer :: Token.GenTokenParser String st Identity
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer -- parses an identifier
reserved   :: String -> Parser ()
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator
parens     :: Parser a -> Parser a
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    :: Parser Integer
integer    = Token.integer    lexer -- parses an integer
semi       :: Parser String
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace
