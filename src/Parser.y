{
module Parser where

import Data.List.Split (wordsBy)
import ADT
import Lexer
}

%name parse t2
%name parseCmd cmd
%tokentype { Token }
%error { parseError }

%token
    lambda  { TokenLambda }
    var     { TokenVar $$ }
    '.'     { TokenDot }
    '('     { TokenOB }
    ')'     { TokenCB }
    '='     { TokenEq }

%%

t2 :: { NamedTerm }
t2  : abs               { $1 }
    | t1 abs            { NamedAppTerm $1 $2 }
    | t1                { $1 }

t1 :: { NamedTerm }
t1  : t1 t0             { NamedAppTerm $1 $2 }
    | t0                { $1 }

t0 :: { NamedTerm }
t0  : var               { Var (Id $1) }
    | '(' t2 ')'        { $2 }

abs :: { NamedTerm }
abs : lambda var '.' t2 { NamedAbsTerm (Id $2) $4 }

cmd :: { Command }
cmd : var '=' t2        { AssignCmd (Id $1) $3 }
    | t2                { EvalCmd $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseCmds :: [Token] -> [Command]
parseCmds ts = map parseCmd $ wordsBy (== TokenNL) ts
}
