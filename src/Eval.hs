module Eval where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import ADT

type Context = Map.Map Id Term

-- | Substitute a closed term
subst :: Context -> Term -> Term
subst m t@(NamedVar x) = fromMaybe t (Map.lookup x m)
subst m (AppTerm s t) = AppTerm (subst m s) (subst m t)
subst m (AbsTerm t) = AbsTerm (subst m t)
subst _ v = v

shift :: Int -> Term -> Term
shift d = shift' 0
    where
        shift' d' (IndexedVar k)
            | k > d' = IndexedVar (k - d' + d)
        shift' d' (AppTerm s t) = AppTerm (shift' d' s) (shift' d' t)
        shift' d' (AbsTerm t) = AbsTerm (shift' (d' + 1) t)
        shift' _ v = v

betaRed :: Term -> Term -> Term
betaRed (AbsTerm t) u = betaRed' t 1
    where
        betaRed' v@(NamedVar _) _ = v
        betaRed' v@(IndexedVar k) n   | k == n    = shift (n - 1) u
                                    | k > n     = IndexedVar (k - 1)
                                    | otherwise = v
        betaRed' (AppTerm s t) n = AppTerm (betaRed' s n) (betaRed' t n)
        betaRed' (AbsTerm t) n = AbsTerm (betaRed' t (n + 1))
betaRed t _ = t

isValue :: Term -> Bool
isValue (AbsTerm _) = True
isValue _ = False

isRedux :: Term -> Bool
isRedux (AppTerm (AbsTerm _) _) = True
isRedux _ = False

callByName :: Term -> Term
callByName t = fromMaybe t (eval t)
    where
        eval :: Term -> Maybe Term
        eval r@(AppTerm s t)
            | isRedux r = eval $ betaRed s t
            | otherwise = step s >>= \s' -> eval $ AppTerm s' t
        eval _ = Nothing

callByValue :: Term -> Term
callByValue t = fromMaybe t (eval t)
    where
        eval :: Term -> Maybe Term
        eval r@(AppTerm s t)
            | isValue s && isValue t    = eval $ betaRed s t
            | isValue s                 = step t >>= \t' -> eval $ AppTerm s t'
            | otherwise                 = step s >>= \s' -> eval $ AppTerm s' t
        eval _ = Nothing

iterateUntilStabilized :: (a -> Maybe a) -> a -> a
iterateUntilStabilized f x = fromJust $ last $ takeWhile isJust
                                $ iterate (maybe Nothing f) (Just x)

-- | A step in the normalization process
step :: Term -> Maybe Term
step r@(AppTerm s t)
    | isRedux r = Just $ betaRed s t
    | otherwise = maybe (step t >>= \t' -> Just $ AppTerm s t')
                        (\s' -> Just $ AppTerm s' t)
                        (step s)
step (AbsTerm t) = step t >>= \t' -> Just $ AbsTerm t'
step _ = Nothing

normalize :: Term -> Term
normalize = iterateUntilStabilized step
