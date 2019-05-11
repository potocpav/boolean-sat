
#include <cryptominisat5/cryptominisat_c.h>


bool cmsat_solve_wrapper(SATSolver* self);

bool cmsat_solve_with_assumptions_wrapper(SATSolver* self, const c_Lit* assumptions, size_t num_assumptions);

slice_lbool *cmsat_get_model_wrapper(const SATSolver* self);

slice_Lit *cmsat_get_conflict_wrapper(const SATSolver* self);
