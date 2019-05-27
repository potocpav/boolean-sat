{- |
Module      :  $Header$
Description :  CNF (Conjunctive Normal Form) formula data type, usilities, and DIMACS export
-}

module Algebra.SAT.CNF (CNF(..), numVars, numClauses, numLiterals, dimacs) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M


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


-- | Convert an expression in CNF into a
-- <http://www.domagoj-babic.com/uploads/ResearchProjects/Spear/dimacs-cnf.pdf DIMACS>
-- text format.
--
-- Useful for interfacing with external SAT solvers.
dimacs :: CNF a -> String
dimacs (CNF e nVars vars) = unlines $
    ("p cnf " ++ show nVars ++ " " ++ show (length e)) :
    map (\vs -> unwords (map show vs) ++ " 0") e
