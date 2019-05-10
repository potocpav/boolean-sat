{-# LANGUAGE InstanceSigs #-}

module Algebra.SAT
    ( Expr(..)
    , cnf
    , solve
    , solveCnf
    , dimacs
    , dimacsCnf
    ) where

import Prelude hiding (not)

import           Data.Maybe
import           Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Algebra.Boolean (Boolean(..))
import           Control.Monad.Supply
-- import           Data.Algebra.Boolean.Negable (Negable(..))
import           SAT.Mios (CNFDescription(..), solveSAT)



data Expr a = Var a
            | Not (Expr a)
            | And (Expr a) (Expr a)
            | Or (Expr a) (Expr a)
            | T
            | F
          deriving (Show)


data CNF a = CNF [[Int]] Int (S.Set a)


instance Boolean (Expr a) where
    true = T
    false = F
    not a = Not a
    a || b = Or a b
    a && b = And a b

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var a) = Var (f a)
    fmap f (Not e) = Not (fmap f e)
    fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
    fmap f (Or e1 e2) = Or (fmap f e1) (fmap f e2)
    fmap f T = T
    fmap f F = F

instance Foldable Expr where
    foldMap :: Monoid m => (a -> m) -> Expr a -> m
    foldMap f (Var a) = f a
    foldMap f (Not e) = foldMap f e
    foldMap f (And e1 e2) = foldMap f e1 <> foldMap f e2
    foldMap f (Or e1 e2) = foldMap f e1 <> foldMap f e2
    foldMap f T = mempty
    foldMap f F = mempty



class Negable a where
    neg :: a -> a


instance Negable Int where
    neg i = -i


varsToInt :: Ord a => Expr a -> (Expr Int, S.Set a)
varsToInt e = ((\v -> S.findIndex v vars + 1) <$> e, vars) where
    vars = foldMap S.singleton e


-- | https://en.wikipedia.org/wiki/Tseytin_transformation
tseytin :: Expr Int -> Int -> ([[Int]], Int)
tseytin e varI = ([v] : l, i-1) where
    ((l, v), i) = runSupply (go e) varI

    go :: Expr Int -> Supply ([[Int]], Int)
    go (Var v) = pure ([], v)
    go (Not e) = do
        (l, a) <- go e
        x <- fresh
        pure ([x, a] : [neg x, neg a] : l, x)
    go (And e f) = do
        (l, a) <- go e
        (m, b) <- go f
        x <- fresh
        pure ([neg x, a] : [neg x, b] : [neg a, neg b, x] : (l ++ m), x)
    go (Or e f) = do
        (l, a) <- go e
        (m, b) <- go f
        x <- fresh
        pure ([neg x, a, b] : [neg a, x] : [neg b, x] : (l ++ m), x)
    go T = do
        x <- fresh
        pure ([[x]], x)
    go F = do
        x <- fresh
        pure ([[neg x]], x)


cnf :: Ord a => Expr a -> CNF a
cnf e = let
    (e', vars) = varsToInt e
    (cnf', nVarsTotal) = tseytin e' (length vars + 1)
    in CNF cnf' nVarsTotal vars


solve :: Ord a => Expr a -> IO (Maybe (M.Map a Bool))
solve = solveCnf . cnf


solveCnf :: Ord a => CNF a -> IO (Maybe (M.Map a Bool))
solveCnf (CNF cnf' nVarsTotal vars) = do
    let descr = CNFDescription nVarsTotal (length cnf') "file"
    l <- solveSAT descr cnf'
    pure $ if length l > 0
        then Just . M.fromList $ zip (S.toList vars) (map (>0) l)
        else Nothing

dimacs :: Ord a => Expr a -> String
dimacs = dimacsCnf . cnf


dimacsCnf :: CNF a -> String
dimacsCnf (CNF e nVars vars) = unlines $
    ("p cnf " ++ show nVars ++ " " ++ show (length e)) :
    map (\vs -> intercalate " " (map show vs) ++ " 0") e
