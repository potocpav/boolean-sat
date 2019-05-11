
#include <stdlib.h>

#include "cryptominisat_c.h"


bool cmsat_solve_wrapper(SATSolver* self) {
    return cmsat_solve(self).x;
}

bool cmsat_solve_with_assumptions_wrapper(SATSolver* self, const c_Lit* assumptions, size_t num_assumptions) {
    return cmsat_solve_with_assumptions(self, assumptions, num_assumptions).x;
}

slice_lbool *cmsat_get_model_wrapper(const SATSolver* self) {
    slice_lbool *res = malloc(sizeof(slice_lbool));
    *res = cmsat_get_model(self);
    return res;
}

slice_Lit *cmsat_get_conflict_wrapper(const SATSolver* self) {
    slice_Lit *res = malloc(sizeof(slice_Lit));
    *res = cmsat_get_conflict(self);
    return res;
}
