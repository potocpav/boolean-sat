{- |
Module      :  $Header$
Description :  Mios SAT solver interface
-}

module Algebra.SAT.Solvers.Mios where

import qualified Data.Set as S
import qualified Data.Map as M
import           SAT.Mios (CNFDescription(..), solveSAT)
import Algebra.SAT.Expr


-- | Use the <http://hackage.haskell.org/package/mios Mios> SAT solver to
-- find a model of a given CNF expression. Returns 'Nothing' if the problem
-- doesn't have a model (is unsatisfiable), otherwise returns 'Just' an
-- arbitrary model.
solve :: Ord a => CNF a -> IO (Maybe (M.Map a Bool))
solve (CNF cnf' nVarsTotal vars) = do
    let descr = CNFDescription nVarsTotal (length cnf') "file"
    print $ "total vars: " <> show nVarsTotal
    l <- solveSAT descr cnf'

    pure $ if length l > 0
        then Just . M.fromList $ zip (S.toList vars) (map (>0) l)
        else Nothing
