{- |
Module      :  $Header$
Description :  CryptoMiniSat high-level interface

High-level wrapper of the <https://github.com/msoos/cryptominisat CryptoMiniSat>
solver.
-}

module Algebra.SAT.Solvers.CryptoMiniSat
    ( solve
    ) where

import Algebra.SAT.Solvers.CryptoMiniSatFFI as FFI
import Algebra.SAT.Expr (CNF(..), Expr, cnf)
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Foreign.Marshal.Array (allocaArray, pokeArray, peekArray)
import Foreign.Storable (peek)
import Foreign.C.Types (CUInt)
import GHC.Conc (getNumCapabilities)


-- | Convert a variable index from the positive/negative convention (negative
-- numbers represent negative atoms) to an unsigned convention, where
-- 'a `mod` 2 == 0' represents negative atoms, and 'a `mod` 2 == 1' represents
-- positive atoms.
convertVar :: Int -> CLit
convertVar i | i > 0 = CLit . fromIntegral $ (i-1) * 2 + 1
             | i < 0 = CLit . fromIntegral $ (-i-1) * 2
             | otherwise = error "Var mustn't be zero"


-- | Use the <https://github.com/msoos/cryptominisat CryptoMiniSat> solver to
-- find a model of a given CNF expression. Returns 'Nothing' if the problem
-- doesn't have a model (is unsatisfiable), otherwise returns 'Just' an
-- arbitrary model.
solve :: Ord a => CNF a -> IO (Maybe (M.Map a Bool))
solve (CNF cnf' nVarsTotal vars) = do
    solver <- cmsat_new
    cmsat_set_num_threads solver =<< fromIntegral <$> getNumCapabilities
    cmsat_new_vars solver (fromIntegral nVarsTotal)
    allocaArray 3 $ \buffer ->
        forM_ cnf' $ \clause -> do
            pokeArray buffer $ map convertVar clause
            cmsat_add_clause solver buffer (fromIntegral $ length clause)
    result <- cmsat_solve_wrapper solver
    modelPtr <- cmsat_get_model_wrapper solver
    model <- peek modelPtr
    c_arr <- peekArray (fromIntegral $ bool_num_vals model) (bool_vals model)
    let arr = map bool_x c_arr
    cmsat_free solver
    free_wrapper modelPtr

    pure $ if result == 0
        then Just . M.fromList $ zip (S.toList vars) (map (>0) arr)
        else Nothing
