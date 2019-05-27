{-# LANGUAGE InstanceSigs #-}

{- |
Module      :  $Header$
Description :  Boolean expression data type
-}

module Algebra.SAT.Expr
    ( Expr(..)
    , CNF(..)
    , cnf
    , dimacsExpr
    ) where

import Prelude hiding (not)

-- import           Data.Maybe
import           Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Algebra.Boolean (Boolean(..))
import           Data.Foldable (fold)
import           Control.Monad.Supply (Supply, fresh, runSupply)
import           Algebra.SAT.CNF (CNF(..), dimacs)
import           Control.Monad (join)


-- | Boolean expression data type.
--
-- The expression usually contains variables satisfying the `Ord` constraint.
--
-- Usually, it is more comfortable to construct an expression using generic
-- `Boolean` functions than using constructors directly (the only exception
-- being the `Var` constructor).
--
-- The `Expr` type could be made much simpler by omitting some constructors,
-- as there are multiple ways to write the same formula:
--
-- > Or a b == Orr [a, b]
-- > And a b == And [a, b]
-- > T == Andd []
-- > F == Orr []
--
-- A richer structure is good for debugging and pretty-printing, but increases
-- code duplication. The list constructors (`Andd`, `Orr`) create clauses with
-- N>3 literals, so the output may not be suitable to 3-SAT solvers.
data Expr a = Var !a                     -- ^ Logical variable
            | Not !(Expr a)
            | And !(Expr a) !(Expr a)
            | Or !(Expr a) !(Expr a)
            | Orr ![Expr a]              -- ^ Disjunction of N `Expr`s
            | Andd ![Expr a]             -- ^ Conjunction of N `Expr`s
            | T                         -- ^ True
            | F                         -- ^ False
          deriving (Show)

-- | Extra logical functions and infix operators
instance Boolean (Expr a) where
    true = T
    false = F
    a || b = Or a b
    a && b = And a b
    or = Orr . foldMap pure
    and = Andd . foldMap pure
    not x = Not x

    -- -- | Convert to CNF right away
    -- not T = F
    -- not F = T
    -- not (Not a) = a
    -- not (Or a b) = And (not a) (not b)
    -- not (And a b) = Or (not a) (not b)
    -- not (Orr aa) = Andd (not <$> aa)
    -- not (Andd aa) = Orr (not <$> aa)
    -- not (Var a) = Not (Var a)


instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var a) = Var (f a)
    fmap f (Not e) = Not (fmap f e)
    fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
    fmap f (Or e1 e2) = Or (fmap f e1) (fmap f e2)
    fmap f (Orr es) = Orr (fmap f <$> es)
    fmap f (Andd es) = Andd (fmap f <$> es)
    fmap f T = T
    fmap f F = F


instance Foldable Expr where
    foldMap :: Monoid m => (a -> m) -> Expr a -> m
    foldMap f (Var a) = f a
    foldMap f (Not e) = foldMap f e
    foldMap f (And e1 e2) = foldMap f e1 <> foldMap f e2
    foldMap f (Or e1 e2) = foldMap f e1 <> foldMap f e2
    foldMap f (Orr es) = fold (foldMap f <$> es)
    foldMap f (Andd es) = fold (foldMap f <$> es)
    foldMap f T = mempty
    foldMap f F = mempty


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
    -- | Optimization: do not introduce variables for a negative literals.
    go (Not (Var v)) = pure ([], -v)
    -- go (Not (Not e)) = go e
    go (Not e) = do
        (l, a) <- go e
        x <- fresh
        pure ([x, a] : [-x, -a] : l, x)
    go (Orr ee) = do
        (ll, aa) <- unzip <$> mapM go ee
        x <- fresh
        pure ((-x : aa) : map (\a -> [-a, x]) aa ++ join ll, x)
    go (Andd ee) = do
        (ll, aa) <- unzip <$> mapM go ee
        x <- fresh
        pure ((x : map (0-) aa) : map (\a -> [a, -x]) aa ++ join ll, x)
    go (And e f) = go (Andd [e, f])
    go (Or e f) = go (Orr [e, f])
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
