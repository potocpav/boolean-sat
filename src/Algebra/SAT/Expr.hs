{-# LANGUAGE InstanceSigs #-}

{- |
Module      :  $Header$
Description :  Boolean expression data type, CNF conversion, and DIMACS export
-}

module Algebra.SAT.Expr
    ( Expr(..)
    , CNF(..)
    , cnf
    , dimacs
    , dimacsExpr
    ) where

import Prelude hiding (not)

import           Data.Maybe
import           Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Algebra.Boolean (Boolean(..))
import           Control.Monad.Supply


-- | Boolean expression data type.
--
-- The expression usually contains variables satisfying the 'Ord' constraint.
--
-- Usually, it is more comfortable to construct an expression using generic
-- 'Boolean' functions than using constructors directly (the only exception
-- being the `Var` constructor).
data Expr a = Var a                     -- ^ Logical variable
            | Not (Expr a)
            | And (Expr a) (Expr a)
            | Or (Expr a) (Expr a)
            | T                         -- ^ True
            | F                         -- ^ False
          deriving (Show)

-- | Extra logical functions and infix operators
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


-- | <https://en.wikipedia.org/wiki/Conjunctive_normal_form Conjunctive normal form (CNF)>
-- boolean expression representation.
--
-- Variables are represented by non-zero integers, where positive numbers are
-- positive literals, and negative numbers are negative literals. The absolute
-- value determines atom's identity.
-- User-defined variables are stored in a 'Data.Set.Set', and their ordering is used
-- to identify them with the lowest integers ([1,2,3,..]) in the clause list.
data CNF a = CNF
    [[Int]]   -- ^ Clause list
    Int       -- ^ Number of logical variables
    (S.Set a) -- ^ Set of user-defined variables


-- | Convert variables in an expression to integral format, and produce
-- a set containing all defined variables. The sorted list can be used to
-- reproduce original variables from integral indices.
varsToInt :: Ord a => Expr a -> (Expr Int, S.Set a)
varsToInt e = ((\v -> S.findIndex v vars + 1) <$> e, vars) where
    vars = foldMap S.singleton e


-- | Convert a boolean expression to CNF using
-- <https://en.wikipedia.org/wiki/Tseytin_transformation Tseytin transformation>
tseytin :: Expr Int -> Int -> ([[Int]], Int)
tseytin e varI = ([v] : l, i-1) where
    ((l, v), i) = runSupply (go e) varI

    go :: Expr Int -> Supply ([[Int]], Int)
    go (Var v) = pure ([], v)
    go (Not e) = do
        (l, a) <- go e
        x <- fresh
        pure ([x, a] : [-x, -a] : l, x)
    go (And e f) = do
        (l, a) <- go e
        (m, b) <- go f
        x <- fresh
        pure ([-x, a] : [-x, b] : [-a, -b, x] : (l ++ m), x)
    go (Or e f) = do
        (l, a) <- go e
        (m, b) <- go f
        x <- fresh
        pure ([-x, a, b] : [-a, x] : [-b, x] : (l ++ m), x)
    go T = do
        x <- fresh
        pure ([[x]], x)
    go F = do
        x <- fresh
        pure ([[-x]], x)


-- | Convert a boolean expression to CNF using
-- <https://en.wikipedia.org/wiki/Tseytin_transformation Tseytin transformation>
cnf :: Ord a => Expr a -> CNF a
cnf e = let
    (e', vars) = varsToInt e
    (cnf', nVarsTotal) = tseytin e' (length vars + 1)
    in CNF cnf' nVarsTotal vars


-- | Convert a boolean expression into a
-- <http://www.domagoj-babic.com/uploads/ResearchProjects/Spear/dimacs-cnf.pdf DIMACS>
-- text format.
--
-- Useful for interfacing with external SAT solvers.
dimacsExpr :: Ord a => Expr a -> String
dimacsExpr = dimacs . cnf


-- | Convert an expression in CNF into a
-- <http://www.domagoj-babic.com/uploads/ResearchProjects/Spear/dimacs-cnf.pdf DIMACS>
-- text format.
--
-- Useful for interfacing with external SAT solvers.
dimacs :: CNF a -> String
dimacs (CNF e nVars vars) = unlines $
    ("p cnf " ++ show nVars ++ " " ++ show (length e)) :
    map (\vs -> intercalate " " (map show vs) ++ " 0") e
