
#include <cryptominisat5/cryptominisat_c.h>

// This thin wrapper is necessary as GHC doesn't support returning structures
// from functions yet. Only "marshallable foreign types" can be returned (raw
// or in IO), so the structs must be passed as pointers instead.
//
// More info is available on the Haskell wiki:
// https://wiki.haskell.org/Foreign_Function_Interface

bool cmsat_solve_wrapper(SATSolver* self);

bool cmsat_solve_with_assumptions_wrapper(SATSolver* self, const c_Lit* assumptions, size_t num_assumptions);

slice_lbool *cmsat_get_model_wrapper(const SATSolver* self);

slice_Lit *cmsat_get_conflict_wrapper(const SATSolver* self);
