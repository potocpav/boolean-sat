
module Algebra.SAT.Solver where

import Algebra.SAT.FFI as FFI
import Algebra.SAT.SAT (CNF(..), Expr, cnf)
import Control.Monad (forM_)
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign.Marshal.Array (allocaArray, pokeArray, peekArray)
import Foreign.Storable (peek)
import Foreign.C.Types (CUInt)


satSolve :: Ord a => Expr a -> IO (Maybe (M.Map a Bool))
satSolve = satSolveCnf . cnf


convertVar :: Int -> CLit
convertVar i | i > 0 = CLit . fromIntegral $ (i-1) * 2 + 1
             | i < 0 = CLit . fromIntegral $ (-i-1) * 2
             | otherwise = error "Var mustn't be zero"


satSolveCnf :: Ord a => CNF a -> IO (Maybe (M.Map a Bool))
satSolveCnf (CNF cnf' nVarsTotal vars) = do
    solver <- cmsat_new
    cmsat_set_num_threads solver 4
    cmsat_new_vars solver (fromIntegral nVarsTotal)
    allocaArray 3 $ \buffer ->
        forM_ cnf' $ \clause -> do
            pokeArray buffer $ map convertVar clause
            cmsat_add_clause solver buffer (fromIntegral $ length clause)
    result <- cmsat_solve_wrapper solver
    model <- peek =<< cmsat_get_model_wrapper solver
    c_arr <- peekArray (fromIntegral $ bool_num_vals model) (bool_vals model)
    let arr = map bool_x c_arr
    cmsat_free solver

    pure $ if result == 0
        then Just . M.fromList $ zip (S.toList vars) (map (>0) arr)
        else Nothing