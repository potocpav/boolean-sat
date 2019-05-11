{-# LANGUAGE ForeignFunctionInterface #-}

module Algebra.SAT.FFI where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Int
import Data.Word

#include "cryptominisat_c.h"


data SATSolver
type Solver = Ptr SATSolver

data CLit = CLit { lit_x :: Int32 }
data CLBool = CLBool { bool_x :: Word8 }
data CSliceLBool = CSliceLBool { bool_vals :: Ptr CLBool, bool_num_vals :: CSize }
data CSliceLit = CSliceLit { lit_vals :: Ptr CLit, lit_num_vals :: CSize }

instance Storable CLit where
    sizeOf _ = 4
    alignment _ = 4
    peek ptr = CLit <$> peekByteOff ptr 0
    poke ptr lit = pokeByteOff ptr 0 (lit_x lit)

instance Storable CLBool where
    sizeOf _ = 1
    alignment _ = 1
    peek ptr = CLBool <$> peekByteOff ptr 0
    poke = undefined

instance Storable CSliceLBool where
    peek ptr = CSliceLBool
        <$> (#peek slice_lbool, vals) ptr
        <*> (#peek slice_lbool, num_vals) ptr

-- instance Storable CSliceLit where
--  peek ptr = CSliceLit <$> peekByteOff ptr 0


foreign import ccall "cmsat_new" cmsat_new
    :: IO Solver
foreign import ccall "cmsat_free" cmsat_free
    :: Solver -> IO ()

foreign import ccall "cmsat_nvars" cmsat_nvars
    :: Solver -> IO CUInt
foreign import ccall "cmsat_add_clause" cmsat_add_clause
    :: Solver -> Ptr CLit -> CSize -> IO Bool
foreign import ccall "cmsat_add_xor_clause" cmsat_add_xor_clause
    :: Solver -> Ptr CUInt -> CSize -> Bool -> IO Bool
foreign import ccall "cmsat_new_vars" cmsat_new_vars
    :: Solver -> CSize -> IO ()

foreign import ccall "cmsat_solve_wrapper" cmsat_solve_wrapper
    :: Solver -> IO CBool
foreign import ccall "cmsat_solve_with_assumptions_wrapper" cmsat_solve_with_assumptions_wrapper
    :: Solver -> Ptr CLit -> CSize -> IO CBool
foreign import ccall "cmsat_get_model_wrapper" cmsat_get_model_wrapper
    :: Solver -> IO (Ptr CSliceLBool)
foreign import ccall "cmsat_get_conflict_wrapper" cmsat_get_conflict_wrapper
    :: Solver -> IO (Ptr CSliceLit)

foreign import ccall "cmsat_set_num_threads" cmsat_set_num_threads
    :: Solver -> CUInt -> IO ()
