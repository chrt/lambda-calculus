module ADT where
import Data.List (elemIndex)

newtype Id = Id String
    deriving (Eq, Show, Ord)

data Term
    = NamedVar Id
    | IndexedVar Int
    | AppTerm Term Term
    | AbsTerm Term
    deriving (Eq, Show)

data NamedTerm
    = Var Id
    | NamedAppTerm NamedTerm NamedTerm
    | NamedAbsTerm Id NamedTerm

data Command = AssignCmd Id NamedTerm
                | EvalCmd NamedTerm
    deriving Show

instance Show NamedTerm where
    show (Var (Id x)) = x
    show (NamedAppTerm s t) = "(" ++ show s ++ " " ++ show t ++ ")"
    show (NamedAbsTerm (Id x) t) = "(λ" ++ x ++ ". " ++ show_t ++ ")"
        where
            show_t = let s = show t in
                if head s == '(' then
                    drop 1 $ take (length s - 1) s
                else
                    s

unname :: NamedTerm -> Term
unname t = unname' t []
    where
        unname' (Var id) ctx = maybe (NamedVar id) (\n -> IndexedVar (n + 1)) $ elemIndex id ctx
        unname' (NamedAppTerm s t) ctx = AppTerm (unname' s ctx) (unname' t ctx)
        unname' (NamedAbsTerm id t) ctx = AbsTerm (unname' t (id : ctx))

name :: [Id] -> Term -> NamedTerm
name ids t = name' t ids 0
    where
        name' (NamedVar id) _ _ = Var id
        name' (IndexedVar n) ids d = Var (ids !! (d - n))
        name' (AppTerm s t) ids d = NamedAppTerm (name' s ids d) (name' t ids d)
        name' (AbsTerm t) ids d = NamedAbsTerm (ids !! d) (name' t ids (d + 1))

namesInTerm :: Term -> [Id]
namesInTerm (NamedVar id) = [id]
namesInTerm (IndexedVar _) = []
namesInTerm (AppTerm s t) = namesInTerm s ++ namesInTerm t
namesInTerm (AbsTerm t) = namesInTerm t

names = map (\c -> Id [c]) "xyztsuv" ++ map (\n -> Id ("x" ++ show n)) [0..]

autoName :: Term -> NamedTerm
autoName t = let used = namesInTerm t in
    name (filter (`notElem` used) names) t
