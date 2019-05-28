{- |
Module      :  $Header$
Description :  Mios SAT solver interface
-}

module Algebra.SAT.Solvers.Mios where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           SAT.Mios (CNFDescription(..), solveSAT)
import Algebra.SAT.Expr


-- | Use the <http://hackage.haskell.org/package/mios Mios> SAT solver to
-- find a model of a given CNF expression. Returns 'Nothing' if the problem
-- doesn't have a model (is unsatisfiable), otherwise returns 'Just' an
-- arbitrary model.
--
-- __Warning:__ there is a <https://github.com/shnarazk/mios/issues/89 bug in Mios>,
-- which sauses various memory corruption errors on large problems. If you
-- encounter any issues, convert to `dimacs` and call the Mios executable
-- on the resulting file, or use a different solver.
solve :: Ord a => CNF a -> IO (Maybe (M.Map a Bool))
solve (CNF cnf' nVarsTotal vars) = do
    let descr = CNFDescription nVarsTotal (length cnf') "file"
    print $ "total vars: " <> show nVarsTotal
    l <- solveSAT descr cnf'

    pure $ if not (null l)
        then Just . M.fromList $ zip (S.toList vars) (map (>0) l)
        else Nothing
