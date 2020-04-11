#include <cstdlib>
#include <cstring>
#include <cmath>
#include <suif1.h>
#include <suifmath.h>
#include "dependence.h"


boolean dependence_library_no_integer_result(const integer_matrix & mat,
                                             boolean * res)
{
    exact ex;
    int fail;
    
    ex.T_constr.init(mat.m(),mat.n());
    
    // To simplify finding integer values for veriables, thus 
    // reducing branch-and-bound calls.  We put variables with
    // largest coefficients last and variables with small
    // coefficients first.  Thus values for variables with large
    // coefficients will be found first.
    // When large variables are set fist, it is easier to find
    // integer solutions for variables with small coefficients
    integer_row cnt(mat.n());
    integer_row ord(mat.n());

    int xj;
    for (int xi=0; xi<mat.m(); xi++)
	for (xj=1; xj<mat.n(); xj++)
	    cnt[xj] = MAX(cnt[xj], ABS(mat.r(xi).c(xj)));
    
    for (xj=0; xj<mat.n(); xj++)
	ord[xj] = xj;
    
    for (int x1=1; x1<mat.n(); x1++)
	for (int x2=x1+1; x2<mat.n(); x2++)
	    if(cnt[x1] > cnt[x2]) {
		int tmp = cnt[x1];
		cnt[x1] = cnt[x2];
		cnt[x2] = tmp;
		tmp = ord[x1];
		ord[x1] = ord[x2];
		ord[x2] = tmp;
	    }
    
    
    for (int a=0; a<mat.m(); a++) 
        for (int c=0; c<mat.n(); c++) 
            ex.T_constr[a][c] = mat.r(a).c(ord[c]);
    
    ex.num_constr = mat.m();
    
    boolean ans = ex.check_bounds(&fail);
    if(res) *res = fail;
    else if (fail) {
        fprintf(stderr, "-------------------\n");
        mat.print(stderr);
        error_line(1, NULL, "Dependence failure checking the read \n");
    } 
    
    return ans;
}


integer_matrix dependence_library_integer_solver(const integer_matrix & mat,
                                                 integer_row * del_list)
{
    lin_ineq L(mat);
    int * red;
    if(del_list) 
        red = del_list->data_array();
    else {
        red = new int[mat.n()];
        for(int i=0; i<mat.n(); i++) red[i] = 1;
    }

    // project
    poly_iterator Poly(L);
    Poly.set_sort_order(red);
    L = Poly.get_iterator(0);
    L = Poly.reduce_extra_constraints2();
    L.sort();

    if(del_list == NULL) delete[] red;
    return L;
}
