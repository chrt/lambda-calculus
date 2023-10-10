module Lexer where

import Data.Char (isAlpha, isDigit, isSpace)

data Token
    = TokenLambda
    | TokenVar String
    | TokenDot
    | TokenOB
    | TokenCB
    | TokenNL
    | TokenEq
    | TokenUnknown Char
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer s@(c:cs)
    | c == '\n' = TokenNL : lexer (dropWhile (== '\n') cs)
    | c == '\\' || c == 'λ' = TokenLambda : lexer cs
    | c == '.' = TokenDot : lexer cs
    | c == '(' = TokenOB : lexer cs
    | c == ')' = TokenCB : lexer cs
    | c == '=' = TokenEq : lexer cs
    | isSpace c = lexer cs
    | isAlpha c = lexVar s
    | otherwise = [TokenUnknown c]

lexVar s = case span (\c -> isAlpha c || isDigit c) s of
    (word, rest) -> TokenVar word : lexer rest
