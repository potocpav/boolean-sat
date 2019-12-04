{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  $Header$
Description :  CryptoMiniSat high-level interface

High-level wrapper of the <https://github.com/msoos/cryptominisat CryptoMiniSat>
solver.
-}

module Algebra.SAT.Solvers.CryptoMiniSat
    ( solve
    ) where

import           Algebra.SAT.Solvers.CryptoMiniSatFFI as FFI
import           Algebra.SAT.Expr (CNF(..), Expr, cnf)
import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Foreign.Marshal.Array (allocaArray, pokeArray, peekArray)
import           Foreign.Storable (peek)
import           Foreign.C.Types (CUInt)
import           GHC.Conc (getNumCapabilities)
import           System.IO.Unsafe (unsafePerformIO)


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
solveIO1 :: Ord a => CNF a -> IO (Maybe (M.Map a Bool))
solveIO1 (CNF cnf' nVarsTotal vars) = do
    solver <- cmsat_new
    cmsat_set_num_threads solver =<< fromIntegral <$> getNumCapabilities
    cmsat_new_vars solver (fromIntegral nVarsTotal)
    let buffer_size = maximum $ map length cnf'
    allocaArray buffer_size $ \buffer ->
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


-- | Use the <https://github.com/msoos/cryptominisat CryptoMiniSat> solver to
-- find a model of a given CNF expression. Returns 'Nothing' if the problem
-- doesn't have a model (is unsatisfiable), otherwise returns 'Just' an
-- arbitrary model.
solveIO :: forall a. Ord a => CNF a -> IO [M.Map a Bool]
solveIO (CNF cnf' nVarsTotal vars) = do
    -- initialize solver
    solver <- cmsat_new
    cmsat_set_num_threads solver =<< fromIntegral <$> getNumCapabilities
    cmsat_new_vars solver (fromIntegral nVarsTotal)
    -- add clauses
    let buffer_size = maximum $ map length cnf'
    allocaArray buffer_size $ \buffer ->
        forM_ cnf' $ \clause -> do
            pokeArray buffer $ map convertVar clause
            cmsat_add_clause solver buffer (fromIntegral $ length clause)

    (solver, solutionM) <- solveIteration solver
    let solutionSeq = iterateM (solveIteration . fst) (solver, absurd)

    -- clean up
    -- cmsat_free solver

    pure $ case solutionM of
        Nothing -> []
        Just s -> [s]
    where

    solveIteration :: Solver -> IO (Maybe (Solver, M.Map a Bool))
    solveIteration solver = do
        -- solve the problem
        result <- cmsat_solve_wrapper solver
        if result /= 0 then cmsat_free solver >> pure Nothing else do
            -- get the model
            modelPtr <- cmsat_get_model_wrapper solver
            model <- peek modelPtr
            c_arr <- peekArray (fromIntegral $ bool_num_vals model) (bool_vals model)
            let arr = map bool_x c_arr
            -- clean up
            free_wrapper modelPtr
            -- return
            pure $ Just (solver, M.fromList $ zip (S.toList vars) (map (>0) arr))



iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f a = iterate (>>= f) (return a)


-- | Pure varian of the solver interface
solve :: Ord a => CNF a -> Maybe (M.Map a Bool)
solve = unsafePerformIO . solveIO1
