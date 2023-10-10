module Main where

import Options.Applicative
import System.IO
import qualified Data.Map.Strict as Map
import Parser
import Lexer
import ADT
import Eval

data Args = Args
    { inputPath     :: Maybe FilePath
    , preludePath   :: Maybe FilePath
    }

args :: Parser Args
args = Args
    <$> optional (argument str
        ( metavar "FILE" ))
    <*> optional (strOption
        ( long "load"
        <> short 'l'
        <> metavar "PRELUDE" ))

main :: IO ()
main = do
    args <- execParser opts
    ctx <- maybe (return Map.empty)
                (\p -> do
                    s <- readFile p
                    return $ fst $ runFromStr Map.empty s
                )
                (preludePath args)
    input <- maybe getContents readFile (inputPath args)
    putStr $ unlines $ snd $ runFromStr ctx input
    where
        opts = info (args <**> helper)
            ( fullDesc
            <> progDesc "Untyped lambda calculus"
            <> header "lambda-calculus - An interpreter for untyped lambda calculus"
            )

runCmd :: Context -> Command -> (Context, String)
runCmd m (AssignCmd x t) = (Map.insert x (subst m $ unname t) m, "")
runCmd m (EvalCmd t) = (m, show $ autoName $ normalize $ subst m $ unname t)

runCmds :: Context -> [Command] -> (Context, [String])
runCmds m [] = (m, [])
runCmds m (cmd:cmds) =
    let (m', s) = runCmd m cmd in
    let (m'', ss) = runCmds m' cmds in
        (m'', s : ss)

runFromStr :: Context -> String -> (Context, [String])
runFromStr ctx = runCmds ctx . parseCmds . lexer
