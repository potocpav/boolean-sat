{- |
Module      :  $Header$

Re-exports of the main library functions, mainly from "Algebra.SAT.Expr".
The CryptoMiniSat solver is used by default; other solvers can be used
directly from their respective modules.
-}

module Algebra.SAT
    ( module Algebra.SAT.Expr
    , module Algebra.SAT.CNF
    , solve
    , solveExpr
    ) where

import           Algebra.SAT.Expr (Expr(..), CNF(..), cnf, dimacsExpr)
import           Algebra.SAT.CNF (CNF(..), dimacs, numClauses, numVars, numLiterals)
import           Algebra.SAT.Solvers.CryptoMiniSat (solve)
import qualified Data.Map.Strict as M


-- | Convenience function for direct expression solving, without the need
-- to convert co CNF first
solveExpr :: Ord a => Expr a -> Maybe (M.Map a Bool)
solveExpr = solve . cnf
