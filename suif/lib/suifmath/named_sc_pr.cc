#define _MODULE_ "libsuifmath.a"

#include <suif1.h>
#include <builder.h>
#include "suifmath.h"
#include <cstdio>
#include <cstring>


/***************************************************************************
 * Print the contents of the ineq.                                         *
 ***************************************************************************/
void named_symcoeff_ineq::print(FILE *fp)
{
    fprintf(fp,"columns : "); cols().print(fp);
    fprintf(fp,"planes  : "); planes().print(fp);
    for(int i=0; i<p(); i++) {
        (*this)[i].print(fp);
        fprintf(fp,"---------\n");
    }
}



/***************************************************************************
 * Number of parts in a given coefficient (ineq r, column c).              *
 * i.e. number of symcoeffs + (integer const != 0)?1:0                     *
 ***************************************************************************/
static boolean n_in_elem(named_symcoeff_ineq & A, int r, int c)
{
    int x = 0;
    for(int i=0; i<A.p(); i++)
        if(A[i][r][c]) x++;
    return x;
}


/***************************************************************************
 * Number of parts that in the summation that forms an inequality.         *
 * i.e. # of parts in the constant term + number of variables with         *
 * non-zero coefficients.                                                  *
 *                                                                         *
 *  ex:                                                                    *
 *      ineq is of the form 1+a+3b +(2c+d)i+ej+(2f+3)k                     *
 *      count this as       + + +      +    +     +                        *
 *      returns 6                                                          *
 ***************************************************************************/
static boolean n_in_ineq(named_symcoeff_ineq & A, int r)
{
    // ineq is of the form 1+a+3b +(2c+d)i+ej+(2f+3)k
    // count this as       + + +      +    +     +
    int x = n_in_elem(A, r, 0);
    for(int i=1; i<A.n(); i++)
        if(n_in_elem(A, r, i)) x++;
    return x;
}



/***************************************************************************
 * Sign of a coefficient.  If all parts of the coefficient of c-th column  *
 * of r-th ineq are positive or negative, return that sign. Otherwise      *
 * return 0.                                                               *
 ***************************************************************************/
static int sign_elem(named_symcoeff_ineq & A, int r, int c)
{
    int s = 0;
    boolean both = FALSE;
    for(int i=0; i<A.p(); i++) {
        if(A[i][r][c] > 0) {
            if(s == -1) both = TRUE;
            s = 1;
        }
        else if(A[i][r][c] < 0) {
            if(s == 1) both = TRUE;
            s = -1;
        }
    }
    if(both) return 0;
    return s;
}


/***************************************************************************
 * Hack to get-around builder and library handling of the variable names.  *
 * Both the suif and builder library insert numbers at the end of a        * 
 * variable name to garuntee uniqueness. This will strip those numbers.    *
 ***************************************************************************/
const char * get_name(name_table_entry & nte)
{
    const char * nm = nte.string();
    return nm;

    /* stuff below doesn't work, so punted -brm
    char gn_buf[32];
    strcpy(gn_buf, nm);

//    for(int i=strlen(gn_buf)-1; i>0; i--) {
//        if((gn_buf[i] >= '0')&&(gn_buf[i] <= '9')) 
//            gn_buf[i] = '\0';
//        else
//            break;
//    }

    return gn_buf;
    */
}
        
    

/***************************************************************************
 * print the coefficient of the c-th column of the r-th ineq.              *
 * First, need to multiply the entire coefficient by the integer value mul *
 * If the coefficient is "1" and dopr is false, do not print anything.     *
 ***************************************************************************/
static boolean print_elem(FILE *fp, named_symcoeff_ineq & A, 
                          int r, int c, 
                          boolean dopr, 
                          int mul=1)
{
    boolean pr = FALSE;
    int v = A[0][r][c]*mul;
    if(v) {
        if((ABS(v) == 1)&&(!dopr)) {
            boolean found = FALSE;
            for(int i=1; (i<A.p())&&(!found); i++) 
                if(A[i][r][c]) found = TRUE;
            if(found) {
                fprintf(fp, "%d", v);
                pr = TRUE;
            }
        } else {
            fprintf(fp, "%d", v);
            pr = TRUE;
        }
    }
    for(int i=1; i<A.p(); i++) {
        const char * nm = get_name(A.planes()[i]);
        v = A[i][r][c]*mul;
        if(v == 1)
            fprintf(fp, "%s%s", (pr)?"+":"", nm);
        else if(v > 0)
            fprintf(fp, "%s%d*%s", (pr)?"+":"", v, nm);
        else if(v==-1) 
            fprintf(fp, "-%s", nm);
        else if(v < 0)
            fprintf(fp, "%d*%s", v, nm);
        if(v) pr = TRUE;
    }
    return pr;
}


/***************************************************************************
 * Print the i-th inequality.                                              *
 ***************************************************************************/
void named_symcoeff_ineq::print_exp(int i, FILE *fp)
{
    boolean pr = n_in_elem(*this, i, 0);
    print_elem(fp, *this, i, 0, TRUE);
    
    for(int j=1; j<n(); j++) {
        int sign = sign_elem(*this, i, j);
        int ne = n_in_elem(*this, i, j);
        
        if(ne) {
            if(sign == -1) fprintf(fp, "-");
            else if(pr) fprintf(fp, "+");
            
            if(ne>1) fprintf(fp, "(");
            boolean pr_str =
                    print_elem(fp, *this, i, j, FALSE, (sign == -1)?-1:1);
            if(ne>1) fprintf(fp, ")");
            fprintf(fp, "%s%s", (pr_str)?"*":"", get_name(cols()[j]));
            pr = TRUE;
        }
    }
    if(!pr) fprintf(fp, "0");
}


/***************************************************************************
 * Print the system of inequalities using multiple formats.                *
 ***************************************************************************/
void named_symcoeff_ineq::print_exp(print_exp_type t, FILE *fp)
{
    if((t != pet_single)&&(t != pet_system_nl)&&(t != pet_expr_nl)) {
        assert(m() == 1);
    }
    for(int i=0; i<m(); i++) {
        if(t == pet_system_nl)
            fprintf(fp, "%3d  ", i);
        print_exp(i, fp);
        if((t == pet_single)||(t == pet_system_nl))
            fprintf(fp, " >= 0\n");
	else if(t == pet_expr_nl)
	    fprintf(fp, "\n");
    }
}


/***************************************************************************
 * Change the sign of a coefficient.                                       *
 ***************************************************************************/
static void change_sign_elem(named_symcoeff_ineq & A, int im, int in)
{
    for(int ip=0; ip<A.p(); ip++)
        A[ip][im][in] *= -1;
}


/***************************************************************************
 * Change the sign of an entire inequality.                                *
 ***************************************************************************/
static void change_sign_ineq(named_symcoeff_ineq & A, int im)
{
    for(int in=0; in<A.n(); in++)
        change_sign_elem(A, im, in);
}


/***************************************************************************
 * Check if the inequality is zero.                                        *
 ***************************************************************************/
static boolean is_zero_ineq(named_symcoeff_ineq & A, int im) {
    for(int ip=0; ip<A.p(); ip++)
        for(int in=0; in<A.n(); in++)
            if(A[ip][im][in]) return FALSE;
    return TRUE;
}


/***************************************************************************
 * Print an expression as a lower or upper bound.  the lsh-th column is    *
 * the left hand side of the expression.                                   *
 * All the divisions are assumed to be integer divfloor functions          *
 *                                                                         *
 * ex:                                                                     *
 *     3+a + Ni + (2-2M)j + 3k >=0   j is the lhs and is an upper bound    *
 *     the result expr is  (3+a + Ni + 3k)/(2M-2)                          *
 *                                                                         *
 * Note that divceil in lower-bound is converted to a divfloor.            *
 *     Ni + (3+M)k >=0   k is the lhs and is an lower bound                *
 *     the result expr is  (-Ni + 2+M)/(3+M)                               *
 ***************************************************************************/
void named_symcoeff_ineq::print_exp(print_exp_type t, int lhs, FILE *fp)
{
    assert((lhs>0)&&(lhs<n()));
   
    named_symcoeff_ineq LX;
    LX.init(p(), m(), 1);
    LX.planes().init(planes());
    named_symcoeff_ineq RX(this);
    
    int ip;
    for(ip=0; ip<p(); ip++) {
        for(int im=0; im<m(); im++) {
            LX[ip][im][0] = (*this)[ip][im][lhs];
            RX[ip][im][lhs] = 0;
        }
    }

    if(m() > 1) fprintf(fp, "%s(", (t==pet_max)?"max":"min");
    for(int im=0; im<m(); im++) {
        int sgnl = sign_elem(LX, im, 0);
        if(sgnl < 0)
            change_sign_elem(LX, im, 0);
        else
            change_sign_ineq(RX, im);

        if(t==pet_max) {
	    int ip;
            for(ip=0; ip<p(); ip++)
                RX[ip][im][0] += LX[ip][im][0];
            RX[0][im][0] -= 1;

            // check if both numerator and denominator can be divided
            // by a constant.
            int g = -1;
            for(ip=0; ip<p(); ip++)
                if(LX[ip][im][0])
                    g = (g==-1)?(ABS(LX[ip][im][0])):gcd(g,ABS(LX[ip][im][0]));
            if(g>1)
                for(ip=0; (ip<p())&&(g>1); ip++)
                    for(int in=0; in<n(); in++)
                        if(RX[ip][im][in])
                            g = gcd(g, ABS(RX[ip][im][in]));
            if(g>1)
                for(ip=0; ip<p(); ip++) {
                    LX[ip][im][0] /= g;
                    for(int in=0; in<n(); in++) 
                        RX[ip][im][in] /= g;
                }
        }

        int nel = n_in_elem(LX, im, 0);
        int ner = n_in_ineq(RX, im);
        assert(nel);

        if((nel == 1)&&(LX[0][im][0] == 1)) nel = 0; // div by 1, don't care

        if(is_zero_ineq(RX, im)) 
            fprintf(fp, "0"); 
        else {
            if(nel && (ner > 1)) fprintf(fp, "(");
            RX.print_exp(im, fp);
            if(nel && (ner > 1)) fprintf(fp, ")");
            
            if(nel) {
                fprintf(fp, "/");
                if(nel > 1) fprintf(fp, "(");
                LX.print_exp(im, fp);
                if(nel > 1) fprintf(fp, ")");
            }
        }

        if(im != m()-1) fprintf(fp, ", ");
    }
    if(m() > 1) fprintf(fp, ")");
}


/***************************************************************************
 * Print the system of ineq. as a loop nest.                               *
 ***************************************************************************/
void named_symcoeff_ineq::print_code(FILE *fp)
{
    print_code(FALSE, 1, n()-1, fp);
}

void named_symcoeff_ineq::print_code(boolean c_format, FILE *fp)
{
    print_code(c_format, 1, n()-1, fp);
}

void named_symcoeff_ineq::print_code(boolean c_format, int st, FILE *fp)
{
    print_code(c_format, st, n()-1, fp);
}

void named_symcoeff_ineq::print_code(boolean c_format, int st, int en,
                                     FILE *fp)
{
    if(n()==0) return;
    assert((st>0)&&(st<n()));
    assert((en>0)&&(en<n()));
    assert(st<=en);
    int tab = 0;
    constraint filt(n());
    int * brackets = new int[n()];
    boolean single_iter_priv = FALSE;

    // BUGBUG: depends on name_table ordering;
    // need to fix this -brm;

    int i;
    for(i=st; i<=en; i++) {
        filt = 0;
        for(int j=i+1; j<n(); j++) filt[j] = 1;
        named_symcoeff_ineq curr = filter_away(&filt, NULL,  0);
        
        filt = 0;
        filt[i] = 1;
        named_symcoeff_ineq lb = curr.filter_thru(&filt, NULL,  1);
        named_symcoeff_ineq ub = curr.filter_thru(&filt, NULL, -1);
        
        //fprintf(fp, "====%d==== LB::\n", i); lb.print_exp(fp);
        //fprintf(fp, "          UB::\n"); ub.print_exp(fp);

        boolean single_iter = FALSE;
        if((lb.m() == 1)&&(ub.m() == 1)) {
            single_iter = TRUE;
            for(int ip=0; (ip<p())&&(single_iter); ip++) {
                // check if lb and ub is there to gard for wholes
                if(ip == 0) {
                    if(ABS(lb[ip][0][i]) > 1) 
                        single_iter = FALSE;       
                } else {
                    if(lb[ip][0][i]) 
                        single_iter = FALSE;       
                }
                        
                // check if ub and lb is not the same
                for(int in=0; in<n(); in++)
                    if(ub[ip][0][in]+lb[ip][0][in] != 0)
                        single_iter = FALSE;
            }
        }

        brackets[i] = -1;
        if(c_format)                               // only in C-format
            if(single_iter)                        // this is single iter
                if(i>st)                           // not the outermost
                    if(!single_iter_priv) {        // priv is not a single iter
                        fprintf(fp, " {");
                        brackets[i] = tab-1;
                    }

        fprintf(fp, "\n");
        for(int x=0; x<tab; x++) fprintf(fp, "   ");

        if(single_iter) {
            fprintf(fp, "%s = ", get_name(cols()[i]));
            ub.print_exp(pet_min, i, fp);
            if(c_format) fprintf(fp, ";");
        } else {
            if(c_format)
                fprintf(fp, "for(%s = ", get_name(cols()[i]));
            else
                fprintf(fp, "for %s = ", get_name(cols()[i]));

            if(lb.m() == 0) 
                fprintf(fp, "-infinity");
            else
                lb.print_exp(pet_max, i, fp);

            if(c_format)
                fprintf(fp, "; %s <= ", get_name(cols()[i]));
            else
                fprintf(fp, " to ");

            if(ub.m() == 0) 
                fprintf(fp, "infinity");
            else
                ub.print_exp(pet_min, i, fp);

            if(c_format)
                fprintf(fp, "; %s++)", get_name(cols()[i]));
                
            tab++;
        }
        single_iter_priv = single_iter;
    }

    for(i=n()-1; i>=st; i--) 
        if(brackets[i] >= 0) {
            fprintf(fp, "\n");
            for(int x=0; x<brackets[i]; x++) fprintf(fp, "   ");
            fprintf(fp, "}");
        }
    
    delete[] brackets;
    fprintf(fp, "\n");
}


//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//--------------- routines for create_expression() ----------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

/***************************************************************************
 * Convert string to var_sym for code generation
 ***************************************************************************/
static var_sym * 
convert_str(const char *nm, base_symtab * symTab)
{
    var_sym * ret;
    base_symtab * base = symTab ? symTab : fileset->globals();

    if (!(ret = base->lookup_var(nm))) {
        ret = (var_sym *) base->new_var(type_signed, nm);

        // if global symtab, must add definition
        // by default, add definition to first file in fileset
        if (!symTab) {
            var_def *def = new var_def(ret, 0);
            fileset->file_list()->head()->contents->symtab()->add_def(def);
        }
    }

    return ret;
}

/***************************************************************************
 * Get all symbolic coefficients of a particular variable in constraint
 ***************************************************************************/
static int 
get_symcoeffs(named_symcoeff_ineq & Lin, int i, int j, 
    block & coeff, int mult, base_symtab * symTab )
{
    // i = constraint
    // j = variable
    // coeff = return value
    // mult = 1 or -1 (depends on lower/upper bound, numerator/denominator)

    int found_coeff = 0;

    for(int c=1; c<Lin.p(); c++) { // iterate over symbolic coeffs
        if (Lin[c][i][j]) {
            found_coeff++;

            // get name of symbolic coefficient

            block symcoeff;
            if(Lin.planes()[c].name().is_symbol()) {
                block var1((var_sym *)Lin.planes()[c].name().symbol());
                symcoeff.set(var1);
            }
            else if (Lin.planes()[c].name().is_string()) {
                const char * nm = Lin.planes()[c].name().string();
                var_sym * v = convert_str(nm, symTab);
                block var1(v);
                symcoeff.set(var1);
            }
            else assert(0);

            // sum with current coefficients
            if (mult*Lin[c][i][j] == 1) {
                if (found_coeff == 1)
                    coeff.set(symcoeff);
                else
                    coeff.set(coeff + symcoeff);
            }
            else if (found_coeff == 1) {
                coeff.set(block(mult*Lin[c][i][j]) * symcoeff);
            }
            else {
                symcoeff.set(block(mult*Lin[c][i][j]) * symcoeff);
                coeff.set(coeff + symcoeff);
            }
        }
    }

    return found_coeff;
}


/***************************************************************************
 * Create expression for upper/lower loop bound
 ***************************************************************************/
instruction * 
named_symcoeff_ineq::create_expression(immed v, boolean is_ub, 
    base_symtab * symTab) const
{
    block exp(0);           // block for bound
    block coeff;            // store coefficients for terms
    int found_sym;          // # of symbolic coefficients discovered
    int mult;               // 1 or -1 (depends on whether processing
                            //  lower/upper bound, numerator/denominator)

    if(symTab == NULL) {
	symTab = const_cols().get_symtab(symTab);
	symTab = const_planes().get_symtab(symTab);
    }

    assert(m() > 0);
    int pos = find_col(v);
    assert(pos);

    constraint filter(n());
    filter = 0;
    int i;
    for (i=pos+1; i<n(); i++)
        filter[i] = 1;
    named_symcoeff_ineq Lin1 = filter_away(&filter, NULL, 0);

    filter = 0;
    filter[pos] = 1;
    named_symcoeff_ineq Lin = Lin1.filter_thru(&filter, NULL, (is_ub)?-1:1);

    for(i=0; i<Lin.m(); i++) {   // iterate over constraints

       mult = is_ub?1:-1;        // negate coeffs for lower bounds

       int term_num = 0;
       block numerator;
       boolean all_zero = TRUE;

       for(int j=0; j<pos; j++) { // iterate over variables & constant
          found_sym = get_symcoeffs(Lin, i, j, coeff, mult, symTab);
          if (found_sym || Lin[0][i][j]) {
              all_zero = FALSE;       // found something
              block term;             // term in sum forming numerator

              if (Lin[0][i][j]) {
                  if (found_sym)
                      coeff.set(coeff + block(mult*Lin[0][i][j]));
                  else
                      coeff.set(block(mult*Lin[0][i][j]));
              }

              if (!j) { // symbolic coeffs for constant term
                  term.set(coeff);
              }
              else if(Lin.cols()[j].name().is_symbol()) {
                  block var((var_sym *)Lin.cols()[j].name().symbol());
                  term.set(coeff * var);
              }
              else if (Lin.cols()[j].name().is_string()) {
                  const char * nm = Lin.cols()[j].name().string();
                  var_sym * v = convert_str(nm, symTab);
                  block var(v);
                  term.set(coeff * var);
              }
              else assert(0);


              if (!term_num++) 
                  numerator.set(term);
              else 
                  numerator.set(numerator + term);
          }
       }

       if (all_zero)
           numerator.set(block(0));
       else {
           mult = -mult;           // negate denominator for upper bound

           block divisor;
           found_sym = get_symcoeffs(Lin, i, pos, divisor, mult, symTab);

           int const_coeff = mult*Lin[0][i][pos];

           if (found_sym) {
               if (const_coeff > 0) 
                   divisor.set(divisor + block(const_coeff));
               else if (mult*Lin[0][i][pos] < 0) 
                   divisor.set(divisor - block(const_coeff));
               numerator.set(block::op(numerator,
                      (is_ub)?bop_divfloor:bop_divceil, divisor));
           }
           else if (const_coeff != 1) {
               divisor.set(block(const_coeff));
               numerator.set(block::op(numerator,
                      (is_ub)?bop_divfloor:bop_divceil, divisor));
           }
       }
       if (i==0)
          exp.set(numerator);
       else
          exp.set(block::op(numerator, (is_ub)?bop_min:bop_max, exp));
    }

    //----------------- debug message ---------------------
    // fprintf(stderr,"%s bound = ", is_ub ? "Upper" : "Lower");
    // exp.print();  
    // fprintf(stderr,"\n");

    instruction * ret = exp.make_instruction((block_symtab *)symTab);
    return ret;
}


// a simple hack
instruction * named_symcoeff_ineq::create_expression(base_symtab * base) const
{
    assert(m() <= 1);
    named_symcoeff_ineq curr(this);
    immed test("__test__");
    curr.add_col(test, n());
    curr[0][0][n()] = -1;

    return curr.create_expression(test, TRUE, base);
}
