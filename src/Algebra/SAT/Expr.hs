{-# LANGUAGE InstanceSigs #-}

{- |
Module      :  $Header$
Description :  Boolean expression data type, CNF conversion, and DIMACS export
-}

module Algebra.SAT.Expr
    ( Expr(..)
    , CNF(..)
    , numVars
    , numClauses
    , numLiterals
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
import           Data.Foldable (fold)
import           Control.Monad.Supply
import           Control.Monad


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
    not = Not
    a || b = Or a b
    a && b = And a b
    or = Orr . foldMap pure
    and = Andd . foldMap pure

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


-- | <https://en.wikipedia.org/wiki/Conjunctive_normal_form Conjunctive normal form (CNF)>
-- boolean expression representation.
--
-- Variables are represented by non-zero integers, where positive numbers are
-- positive literals, and negative numbers are negative literals. The absolute
-- value determines atom's identity.
-- User-defined variables are stored in a 'Data.Set.Set', and their ordering is used
-- to identify them with the lowest integers ([1,2,3,..]) in the clause list.
data CNF a = CNF
    ![[Int]]   -- ^ Clause list
    !Int       -- ^ Number of logical variables
    !(S.Set a) -- ^ Set of user-defined variables


-- | Get the total number of variables, including auto-generated ones
numVars :: CNF a -> Int
numVars (CNF _ i _) = i


-- | Get the total number of clauses in the CNF formula
numClauses :: CNF a -> Int
numClauses (CNF l _ _) = length l


-- | Get the total number of clauses in the CNF formula
numLiterals :: CNF a -> Int
numLiterals (CNF l _ _) = sum $ map length l


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
    go (Orr ee) = do
        (ll, aa) <- unzip <$> mapM go ee
        x <- fresh
        pure ((-x : aa) : map (\a -> [-a, x]) aa ++ join ll, x)
    go (Andd ee) = do
        (ll, aa) <- unzip <$> mapM go ee
        x <- fresh
        pure ((x : map (0-) aa) : map (\a -> [a, -x]) aa ++ join ll, x)
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
    map (\vs -> unwords (map show vs) ++ " 0") e
