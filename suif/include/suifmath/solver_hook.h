typedef boolean (*no_integer_result_hook_f)(const integer_matrix &, boolean *);
typedef integer_matrix (*integer_solver_hook_f)(const integer_matrix &, integer_row *);

extern no_integer_result_hook_f NIR_hook;
extern integer_solver_hook_f    IS_hook;
