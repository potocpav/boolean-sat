{- |
Module      :  $Header$
Description :  CryptoMiniSat low-level interface

Low-level foreign function interface to the
<https://github.com/msoos/cryptominisat CryptoMiniSat> solver. The
<https://github.com/msoos/cryptominisat/blob/master/src/cryptominisat_c.h.in raw C language bindings>
are indirectly called through a
<https://github.com/potocpav/boolean-sat/tree/master/c-wrapper thin C wrapper>.
This is necessary, because the raw functions return C structs by value, which
is not supported by GHC.

Usage of this low-level interface may be inspired by the high-level
wrapper code in "Algebra.SAT.Solvers.CryptoMiniSat".
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Algebra.SAT.Solvers.CryptoMiniSatFFI where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Int
import Data.Word

#include "cryptominisat_c.h"


-- | Opaque structure containing solver state and configuration
data SATSolver
type Solver = Ptr SATSolver

-- | Atom representation. If 'lit_x' is odd, atom is positive, otherwise, atom
-- is negative.
data CLit = CLit { lit_x :: Word32 }
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
    sizeOf _ = (#size slice_lbool)
    alignment _ = (#alignment slice_lbool)
    peek ptr = CSliceLBool
        <$> (#peek slice_lbool, vals) ptr
        <*> (#peek slice_lbool, num_vals) ptr
    poke ptr slice = do
        (#poke slice_lbool, vals) ptr $ bool_vals slice
        (#poke slice_lbool, num_vals) ptr $ bool_num_vals slice


-- instance Storable CSliceLit where
--  peek ptr = CSliceLit <$> peekByteOff ptr 0


foreign import ccall unsafe "cmsat_new" cmsat_new
    :: IO Solver
foreign import ccall unsafe "cmsat_free" cmsat_free
    :: Solver -> IO ()

foreign import ccall unsafe "cmsat_nvars" cmsat_nvars
    :: Solver -> IO CUInt
foreign import ccall unsafe "cmsat_add_clause" cmsat_add_clause
    :: Solver -> Ptr CLit -> CSize -> IO Bool
foreign import ccall unsafe "cmsat_add_xor_clause" cmsat_add_xor_clause
    :: Solver -> Ptr CUInt -> CSize -> Bool -> IO Bool
foreign import ccall unsafe "cmsat_new_vars" cmsat_new_vars
    :: Solver -> CSize -> IO ()

foreign import ccall unsafe "cmsat_solve_wrapper" cmsat_solve_wrapper
    :: Solver -> IO CBool
foreign import ccall unsafe "cmsat_solve_with_assumptions_wrapper" cmsat_solve_with_assumptions_wrapper
    :: Solver -> Ptr CLit -> CSize -> IO CBool
foreign import ccall unsafe "cmsat_get_model_wrapper" cmsat_get_model_wrapper
    :: Solver -> IO (Ptr CSliceLBool)
foreign import ccall unsafe "cmsat_get_conflict_wrapper" cmsat_get_conflict_wrapper
    :: Solver -> IO (Ptr CSliceLit)
foreign import ccall unsafe "free_wrapper" free_wrapper
    :: Ptr a -> IO ()

foreign import ccall unsafe "cmsat_set_num_threads" cmsat_set_num_threads
    :: Solver -> CUInt -> IO ()
