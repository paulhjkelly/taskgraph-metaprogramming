/* file "print_code.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"

#include <cstdlib>
#include <cmath>
#include <cstring>
#include <suif.h>
#include <suifmath.h>
#include "dependence.h"


    
enum print_code_status { PCS_SINGLE, PCS_RANGE, PCS_COND};
struct print_code_e {
    print_code_status status;
    char * var;
    int divvar;
    int stride;
    char * lb;
    char * ub;

    print_code_e() { status = (print_code_status)0; var = 0;
		     lb = 0; ub = 0; divvar = 1; stride = 1;}
    ~print_code_e() { if(var) delete var;
                      if(lb) delete lb;
                      if(ub) delete ub; }

    void init(print_code_e & in) {
        status = in.status;
        var = in.var;
        divvar = in.divvar;
        stride = in.stride;
        lb = in.lb;
        ub = in.ub;
    }
};


/********************************************************************************
 *  print code                                                                  *
 *                                                                              *
 ********************************************************************************/
int lin_ineq_op::print_code(lin_ineq & a, lin_ineq * stride, name_store * nm, FILE * fp, int pr_acc)
{
    return new_print_code(a, stride, nm->params, fp, pr_acc);
}




void lin_ineq_op::single_update(lin_ineq & a, constraint repl, int rnk)
{
/*    lin_ineq rlow(a);
    for(int i = 0; i <a.m(); i++)
        if(rlow[i].rank() > rnk) rlow[i] = 0;
    rlow.del_zeros();

    constraint flt(a.n());
    flt[rnk] = 1;
    lin_ineq got = rlow.filter_thru(flt, 1);
    assert(got.m() == 1); 

    int val = got[0][rnk];
    constraint repl(got[0]); */
    int val = repl[rnk];
    for(int i=0; i<a.m(); i++)
        if(a[i][rnk] != 0) {
            int l = lcm(val, ABS(a[i][rnk]));
            constraint resa = a[i];
            resa *= l;
            resa /= ABS(a[i][rnk]);
            constraint resv = repl;
            resv *= l;
            resv /= val;
            constraint res;
            if(a[i][rnk] > 0) 
                res= resa - resv; // Cando since resv >= 0 and -resv >=0 both in a
            else
                res= resa + resv;
            if(res.rank() > 0) {
                a[i] = res;
                int g = a[i].row_gcd();
                a[i] /= g;
            }
        }
}

void print_constraint(char * buf, char ** name, constraint & curr, int divnum[])
{
    char tmpbuf[32];
    int dosign = 0;
    if(curr[0] != 0) {
        sprintf(tmpbuf, "%d", curr[0]);
        strcat(buf, tmpbuf);
        dosign = 1;
    }
    for(int k=1; k<curr.n(); k++) {
        if(curr[k] > 0) {
            if(dosign) strcat(buf, "+");
            if(curr[k] > 1) {
                sprintf(tmpbuf, "%d", curr[k]);
                strcat(buf, tmpbuf);
                strcat(buf, "*");
            }
            sprintf(tmpbuf, "%s", name[k-1]);
            strcat(buf, tmpbuf);
            dosign = 1;
        }
        else if(curr[k] < 0) {
            if(curr[k] < -1) {
                sprintf(tmpbuf, "%d", curr[k]);
                strcat(buf, tmpbuf);
                strcat(buf, "*");
            } else
                strcat(buf, "-");
            sprintf(tmpbuf, "%s", name[k-1]);
            strcat(buf, tmpbuf);
            dosign = 1;
        }
    }
    if(dosign == 0)
        strcat(buf, "0");
    
    strcat(buf, " >= 0");
}


int lin_ineq_op::print_code(lin_ineq a, lin_ineq * stride, char ** name, FILE * fp, int pr_acc)
{
    char buf[255];
    char tmpbuf[32];
    int  divnum[32];
    int cnt = 0;
    
    for(int i = 0; i<kind.n(); i++) {
        if( kind[i] == NM_LOCATIONS) cnt++; 
        divnum[i] = 1;
    }
    
    print_code_e * code = new print_code_e[cnt*2];
    
    int code_num = -1;
    for(i=0; i < cnt; i++) {
        code_num++;
        
        lin_ineq lo = get_lower(a, i);
        lin_ineq hi = get_upper(a, i);
        
        if((!hi.is_empty())&&(!lo.is_empty())) {
            // All location var's in defining  current location 
            // should be a function of already defined location vars
            int myrank = kind.make_filter(NM_LOCATIONS, i).rank();
            for(int c=0; c< lo.m(); c++)
                if(lo[c].rank() != myrank) lo[c] = 0;
            lo.del_zeros();
            for(c=0; c< hi.m(); c++)
                if(hi[c].rank() != myrank) hi[c] = 0;
            hi.del_zeros();
            
            if((!hi.is_empty())&&(!lo.is_empty())) {
                // If any ineq as an index with coif != 1 then we'll get an lcm != 1
                int div = 1;
                for(int j=0; j < lo.m(); j++)     
                    div = lcm(div, lo[j][myrank]);
                for(j=0; j < hi.m(); j++)     
                    div = lcm(div, -hi[j][myrank]);
                code[code_num].divvar = div;
                
                code[code_num].var = new char[strlen(name[lo[0].rank()-1])+1];
                strcpy(code[code_num].var, name[lo[0].rank()-1]);
                divnum[lo[0].rank()] = div;
                
                if(stride) {
                    constraint st_filt(a.n());
                    st_filt[myrank] = 1;
                    lin_ineq curr_stride = stride->filter_thru(st_filt, -1);
                    if(curr_stride.m() == 0)
                        code[code_num].stride = 1;
                    else {
                        assert(curr_stride.m() == 1);
                        curr_stride[0][curr_stride[0].rank()] = 0;
                        code[code_num].stride = curr_stride[0][curr_stride[0].rank()];
                    }
                } else
                    code[code_num].stride = 1;
                
                int single_assign = 0;
                int found_lo, found_hi=0;
                // Find if var can be expressed as i = .....
                for(found_lo=0; found_lo < lo.m(); found_lo++) {
                    for(found_hi=0; found_hi < hi.m(); found_hi++) {
                        constraint tmp = lo[found_lo] + hi[found_hi];
                        if(tmp.rank() == 0) {                            // Range = 1 
                            if(ABS(tmp[0]) == lo[found_lo][myrank]-1)    // Range is fixed
                                single_assign = 1;
                            
                            // Saman 9/19/92
                            if(ABS(tmp[0]) == 0)                        // Range is i  LB = UB
                                single_assign = 1;
                        }
                        if(single_assign) break;
                    }
                    if(single_assign) break;
                }
                
                //            // BUGBUG:  Make sure no if conditions
                //            if((lo.m() > 1) ||(hi.m() > 1)) single_assign = 0;
                
                if(single_assign) {
                    lo[found_lo][0] = lo[found_lo][myrank]*(int)floor((double)lo[found_lo][0]/(double)lo[found_lo][myrank]);
                    constraint curr = lo[found_lo];
                    
                    strcpy(buf,"");
                    curr *= div/lo[found_lo][myrank];
                    curr *=  -1;
                    curr[curr.rank()] = 0;
                    int dosign = 0;
                    if(curr[0] != 0) {
                        sprintf(tmpbuf, "%d", curr[0]);
                        strcat(buf, tmpbuf);
                        dosign = 1;
                    }
                    for(int k=1; k<curr.n(); k++) {
                        if(curr[k] > 0) {
                            if(dosign) strcat(buf, "+");
                            if(curr[k] > 1) {
                                sprintf(tmpbuf, "%d", curr[k]);
                                strcat(buf, tmpbuf);
                                strcat(buf, "*");
                            }
                            sprintf(tmpbuf, "%s", name[k-1]);
                            strcat(buf, tmpbuf);
                            dosign = 1;
                        }
                        else if(curr[k] < 0) {
                            if(curr[k] < -1) {
                                sprintf(tmpbuf, "%d", curr[k]);
                                strcat(buf, tmpbuf);
                                strcat(buf, "*");
                            } else
                                strcat(buf, "-");
                            sprintf(tmpbuf, "%s", name[k-1]);
                            strcat(buf, tmpbuf);
                            dosign = 1;
                        }
                    }
                    if(dosign == 0)
                        strcat(buf, "0");
                    
                    
                    
                    
                    
                    if((lo.m() > 1) ||(hi.m() > 1)) {
                        code[code_num+1].init(code[code_num]);
                        code[code_num+1].status = PCS_SINGLE;
                        code[code_num+1].lb = new char[strlen(buf)+1];
                        strcpy(code[code_num+1].lb, buf);
                        
                        constraint curr = lo[found_lo];
                        lo[found_lo] = 0;
                        hi[found_hi] = 0;
                        single_update(lo, curr, myrank);
                        single_update(hi, curr, myrank);
                        lo.del_zeros();
                        hi.del_zeros();
                        strcpy(buf,"");        
                        
                        int tot = lo.m() + hi.m();
                        if(tot > 1)
                            strcat(buf,"(");         
                        
                        for(j=0; j<lo.m(); j++) {
                            if(j > 0) strcat(buf, ") && (");
                            print_constraint(buf, name, lo[j], divnum);
                        }
                        
                        for(j=0; j<hi.m(); j++) {
                            if((j > 0)||(lo.m()>0)) strcat(buf, ") && (");
                            print_constraint(buf, name, hi[j], divnum);
                        }
                        
                        if(tot > 1)
                            strcat(buf,")");         
                        
                        code[code_num].status = PCS_COND;
                        code[code_num].lb = new char[strlen(buf)+1];
                        strcpy(code[code_num].lb, buf);
                        code_num++;
                    } else {
                        code[code_num].status = PCS_SINGLE;
                        code[code_num].lb = new char[strlen(buf)+1];
                        strcpy(code[code_num].lb, buf);
                    }
                }
                
                
                if(!single_assign) {
                    code[code_num].status = PCS_RANGE;
                    
                    // Lower bound
                    strcpy(buf,"");
                    
                    for(int j=0; j < lo.m(); j++) {             // All of constraints
                        if(lo.m() > 1) {
                            if(j != 0) strcat(buf, ", ");
                            if(j != lo.m()-1)
                                strcat(buf, "MAX( ");               // Max all the constraints
                        }
                        
                        constraint curr = lo[j];
                        curr *= div/lo[j][myrank];
                        curr *=  -1;
                        curr[curr.rank()] = 0;
                        int dosign = 0;
                        if(curr[0] != 0) {
                            sprintf(tmpbuf, "%d", curr[0]);
                            strcat(buf, tmpbuf);
                            dosign = 1;
                        }
                        for(int k=1; k<curr.n(); k++) {
                            if(curr[k] > 0) {
                                if(dosign) strcat(buf, "+");
                                if(curr[k] > 1) {
                                    sprintf(tmpbuf, "%d", curr[k]);
                                    strcat(buf, tmpbuf);
                                    strcat(buf, "*");
                                }
                                sprintf(tmpbuf, "%s", name[k-1]);
                                strcat(buf, tmpbuf);
                                dosign = 1;
                            }
                            else if(curr[k] < 0) {
                                if(curr[k] < -1) {
                                    sprintf(tmpbuf, "%d", curr[k]);
                                    strcat(buf, tmpbuf);
                                    strcat(buf, "*");
                                } else
                                    strcat(buf, "-");
                                sprintf(tmpbuf, "%s", name[k-1]);
                                strcat(buf, tmpbuf);
                                dosign = 1;
                            }
                        }
                        if(dosign == 0)
                            strcat(buf, "0");
                    }
                    for(j=0; j < lo.m()-1; j++) 
                        strcat(buf, ")");
                    
                    code[code_num].lb = new char[strlen(buf)+1];
                    strcpy(code[code_num].lb, buf);
                    
                    
                    // Upper bound
                    strcpy(buf,"");
                    for(j=0; j < hi.m(); j++) {             // All of constraints
                        if(hi.m() > 1) {
                            if(j != 0) strcat(buf, ", ");
                            if(j != hi.m()-1)
                                strcat(buf, "MIN( ");               // Max all the constraints
                        }
                        
                        constraint curr = hi[j];
                        curr *= -div/hi[j][myrank];
                        curr[curr.rank()] = 0;
                        int dosign = 0;
                        if(curr[0] != 0) {
                            sprintf(tmpbuf, "%d", curr[0]);
                            strcat(buf, tmpbuf);
                            dosign = 1;
                        }
                        for(int k=1; k<curr.n(); k++) {
                            if(curr[k] > 0) {
                                if(dosign) strcat(buf, "+");
                                if(curr[k] > 1) {
                                    sprintf(tmpbuf, "%d", curr[k]);
                                    strcat(buf, tmpbuf);
                                    strcat(buf, "*");
                                }
                                sprintf(tmpbuf, "%s", name[k-1]);
                                strcat(buf, tmpbuf);
                                dosign = 1;
                            }
                            else if(curr[k] < 0) {
                                if(curr[k] < -1) {
                                    sprintf(tmpbuf, "%d", curr[k]);
                                    strcat(buf, tmpbuf);
                                    strcat(buf, "*");
                                } else
                                    strcat(buf, "-");
                                sprintf(tmpbuf, "%s", name[k-1]);
                                strcat(buf, tmpbuf);
                                dosign = 1;
                            }
                        }
                        if(dosign == 0)
                            strcat(buf, "0");
                    }
                    for(j=0; j < hi.m()-1; j++) 
                        strcat(buf, ")");
                    
                    code[code_num].ub = new char[strlen(buf)+1];
                    strcpy(code[code_num].ub, buf);
                    
                }
            }
        }
    }    
    
    
    
    int step = 0;
    for(i=0; i <= code_num; i++) 
        if(code[i].var) {  // Did we not skip this level
            //GROSS HACK needed to generate right code for ../herm/barf.c 2/18/92
            int blk = ((strchr(code[i].var, '_') == 0)&&(strncmp(code[i].var, "proc", 4) == 0))?1:0;
            
            for(int j=0; j <step; j++) printf("   ");
            if(code[i].status == PCS_RANGE) {
                if(code[i].divvar == 1) {
                    if(blk)
                        fprintf(fp, "if((%s >= %s)&&(%s <= %s))\n", 
                                code[i].var, code[i].lb, 
                                code[i].var, code[i].ub);
                    else {
                        fprintf(fp, "for(%s = %s; %s <= %s; ",
                                code[i].var, code[i].lb, 
                                code[i].var, code[i].ub);
                        if(code[i].stride == 1)
                            fprintf(fp, "%s++)\n", code[i].var);
                        else
                            fprintf(fp, "%s += %d)\n", code[i].var, code[i].stride);
                    }
                } else {
                    if(blk)
                        fprintf(fp, "if((%s >= divceil(%s, %d))&&(%s <= divfloor(%s, %d)))\n", 
                                code[i].var, code[i].lb, code[i].divvar, 
                                code[i].var, code[i].ub, code[i].divvar);
                    else {
                        fprintf(fp, "for(int %s = divceil(%s, %d); %s <= divfloor(%s, %d); ",
                                code[i].var, code[i].lb, code[i].divvar, 
                                code[i].var, code[i].ub, code[i].divvar);
                        if(code[i].stride == 1)
                            fprintf(fp, "%s++)\n", code[i].var);
                        else
                            fprintf(fp, "%s += %d)\n", code[i].var, code[i].stride);
                    }
                }
                step++;
            } else if(code[i].status == PCS_COND) {
                fprintf(fp, "if(%s)\n", code[i].lb);
                step++;
            } else {
                if(code[i].divvar == 1) {
                    if(blk) {
                        fprintf(fp, "if(%s == %s);\n", code[i].var, code[i].lb);
                        step++;
                    } else 
                        fprintf(fp, "%s = %s;\n", code[i].var, code[i].lb);
                } else {
                    if(blk) {
                        fprintf(fp, "if(%s == (%s)/%d);\n", code[i].var, code[i].lb, code[i].divvar);
                        step++;
                    } else
                        fprintf(fp, "%s = (%s)/%d;\n", code[i].var, code[i].lb, code[i].divvar); 
                }
            }
        }
    
    
    // 
    if(pr_acc) {
        for(int j=0; j <step; j++) printf("   ");
        fprintf(fp, "A");
        for(i=0; i <= code_num; i++)  {
            fprintf(fp, "[%s]", code[i].var);
        }
        fprintf(fp, " += 1;\n");
    }
    
    
    
}



void print_tab(int step)
{
    for(int j=0; j <step; j++) printf("   ");
}

enum node_op { NO_UNKNOWN, NO_INT,  NO_VAR,  NO_ADD, NO_SUB, 
               NO_MUL,     NO_DIVC, NO_DIVF, NO_MAX, NO_MIN};

class code_tree_node {
friend class code_nest;
    node_op op;
    code_tree_node * left;
    code_tree_node * right;
    code_tree_node * par;
    code_tree_node * sib;
    int val;
    char * var;
    int flat;
public:
    code_tree_node() { init(); }
    code_tree_node(int v) { init(); op = NO_INT; val = v; }
    code_tree_node(char * v) { init(); op = NO_VAR; var = v; }
    code_tree_node(node_op o, code_tree_node * l, code_tree_node * r) {
        init();
        add(o, l, r);
    }

    void init() { op = NO_UNKNOWN; par = left = right = sib = NULL; var = 0; flat = 0; }

    void add(node_op o, code_tree_node * l, code_tree_node * r) {
        op = o;
        left = l;
        right = r;
        l->par = this;
        r->par = this;
    }

    void delete_me();
    void absorb(code_tree_node * x);
    void change_par(code_tree_node * to, code_tree_node * from);
    void flatten(code_tree_node * opNode = NULL);
    void optimize0();
    void optimize1();
    void optimize2();
    void do_optimize();

    void op_str(char *);
    void print(FILE * fp);
};

class code_nest {
friend class lin_ineq_op;
    code_nest * sib;
    code_nest * par;
    char * var;
    int eq;
    int step;
    code_tree_node * lb;
    code_tree_node * ub;
public:
    code_nest() { sib = par = NULL; lb = ub = NULL; var = 0; eq = 0; step = 1; }
    code_nest * make_sib();
    int print(FILE * fp=stdout, int tab=0);

    code_tree_node * create_code_tree(constraint & val, char ** name, int lev, node_op o);
    code_tree_node * create_code_tree(lin_ineq & val, char ** name, int lev, node_op o);
};



/* ##################################################
   #####   code_tree_node                       #####
   ################################################## */


/********************************************************************************
 * delete the current node from a flattened tree                                *
 * don't worry about children since they are also nuked                         *
 ********************************************************************************/
void code_tree_node::delete_me()
{
    assert(flat);
    assert(par);

    if(this == par->left) {
        par->left = sib;
    } else {
        code_tree_node * curr = par->left;
        while(curr->sib != this) {
            curr = curr->sib;
            assert(curr);
        }
        curr->sib = sib;
    }
        
}

/********************************************************************************
 * move left child to me.  This is done when my instruction is irravalent       *
 * (like add with only one parameter left-can happen when flattened             *
 * make sure all the siblings of left know that I am their parent now           *
 ********************************************************************************/
void code_tree_node::absorb(code_tree_node * x)
{
    left->change_par(this, left);

    op = x->op;
    val = x->val;
    var = x->var;

    assert(x->sib == 0);
    left = x->left;
}

/********************************************************************************
 * if this node and all the child nodes has from as the parent change it to to  *
 ********************************************************************************/
void code_tree_node::change_par(code_tree_node * to, code_tree_node * from)
{
    if(par == from) par = to;

    if(sib) sib->change_par(to, from);
    if(left) left->change_par(to, from);
}


/********************************************************************************
 *  str get the operation of the node                                           *
 ********************************************************************************/
void code_tree_node::op_str(char * str)
{
    
    switch(op) {
    case NO_UNKNOWN:
        sprintf(str, "?");
        break;
    case NO_INT:
        sprintf(str, "%d", val);
        break;
    case NO_VAR:
        sprintf(str, "%s", var);
        break;
    case NO_ADD:
        sprintf(str, "+");
        break;
    case NO_SUB:
        sprintf(str, "-");
        break;
    case NO_MUL:
        sprintf(str, "*");
        break;
    case NO_DIVC:
        sprintf(str, "divceil");
        break;
    case NO_DIVF:
        sprintf(str, "divfloor");
        break;
    case NO_MAX:
        sprintf(str, "MAX");
        break;
    case NO_MIN:
        sprintf(str, "MIN");
        break;
    };
}


/********************************************************************************
 *  flatten tree                                                                *
 *  for commutable instructions (+, *, MAX) instead of each instruction has two *
 *  operands(left, right) and a tree of commutable op's, create a single op     *
 *  left child and it's siblings(sib chain) points to all these nodes.          *
 ********************************************************************************/
void code_tree_node::flatten(code_tree_node * opNode)
{
    assert(flat == 0);
    flat = 1;

    if(opNode) {
        if(opNode->op == op) {
            if(left) left->flatten(opNode);
            if(right) right->flatten(opNode);
        } else {
            if(opNode->left == NULL) {
                opNode->left = this;
            } else {        
                code_tree_node * curr = opNode->left;
                while(curr->sib) curr = curr->sib;
                curr->sib = this;
            }       
            sib = NULL;
            par = opNode;

            code_tree_node * tmpleft = left;
            left = NULL;
            if(tmpleft) tmpleft->flatten(this);
            if(right) right->flatten(this);
            right = NULL;
        } 
    } else {
        code_tree_node * tmpleft = left;
        left = NULL;
        if(tmpleft) tmpleft->flatten(this);
        if(right) right->flatten(this);
        right = NULL;
    }
}



/********************************************************************************
 *  optimization 0                                                              *
 *  multiple integers in *, + and - chains is concatinated to a single          *
 *  divceil and divfloor is an integer dividing another int(answer known)       *
 *   or divident is a multiple of divisor(divident changed)                     *
 ********************************************************************************/
void code_tree_node::optimize0()
{
    assert(flat);
    
    if(left) left->optimize0();
    if(sib) sib->optimize0();
    
    //    print(stdout); printf("\n");
    
    
    switch(op) {
    case NO_UNKNOWN:
    case NO_INT:
    case NO_VAR:
        break;
        
    case NO_ADD:
    case NO_SUB:
    case NO_MUL:
        int value = (op==NO_MUL)?1:0;
        code_tree_node * curr = left;
        code_tree_node * first_int =  NULL;
        while(curr) {
            if(curr->op == NO_INT) {
                value = (op==NO_MUL)?(value*curr->val):(value+curr->val);
                if(first_int == NULL) {
                    first_int = curr;
                    curr = curr->sib;
                } else {
                    code_tree_node * tmp = curr;
                    curr = curr->sib;
                    tmp->delete_me();
                }
            } else
                curr = curr->sib;
                
        }
        if(first_int) first_int->val = value;
        break;
        
    case NO_DIVC:
    case NO_DIVF:
        assert(left);
        assert(left->sib);
        assert(left->sib->sib == 0);
        assert(left->sib->op == NO_INT);
        if(left->op == NO_INT) {
            if(op == NO_DIVC)
                val =  (int)ceil((double)left->val/(double)left->sib->val);
            else
                val =  (int)floor((double)left->val/(double)left->sib->val);
            op = NO_INT;
            left = NULL;
        } else if(left->op ==  NO_MUL) {
            code_tree_node * curr = left->left;
            code_tree_node * first_int =  NULL;
            while(curr) {
                if(curr->op == NO_INT) {
                    if(first_int == NULL) 
                        first_int = curr;
                    else
                        assert(0);
                }
                curr = curr->sib;
            }

            if(first_int->val % left->sib->val == 0) {
                first_int->val /= left->sib->val;
                left->sib = NULL;
                absorb(left);
            }
        }

        break;

    case NO_MAX:
    case NO_MIN:
        
        break;
    }


}




/********************************************************************************
 *  optimization 1                                                              *
 *  adding a zero is a noop                                                     *
 *  multiplying by 1 is a noop                                                  *
 *  one operand is a noop                                                       *
 ********************************************************************************/
void code_tree_node::optimize1()
{
    assert(flat);
    
    if(left) left->optimize1();
    
    if(sib) sib->optimize1();
    
    //    print(stdout); printf("\n");
    
    
    if(par)
        switch(par->op) {
        case NO_UNKNOWN:
        case NO_INT:
        case NO_VAR:
            assert(0);
            break;
            
        case NO_ADD:
        case NO_SUB:
            if(op == NO_INT) 
                if(val == 0)
                    /* I am not the only one */
                    if(!((par->left == this)&&(sib == NULL)))
                        delete_me();
            
            
            
            break;
            
        case NO_MUL:
            if(op == NO_INT)
                if(val == 1)
                    if(!((par->left == this)&&(sib == NULL)))
                        delete_me();
            break;
            
        case NO_DIVC:
        case NO_DIVF:
        case NO_MAX:
        case NO_MIN:
            
            break;
        }
    
    
    if(left)
        if(left->sib == 0)  
            absorb(left);
    
}


/********************************************************************************
 *  optimization 2                                                              *

 ********************************************************************************/
void code_tree_node::optimize2()
{
    assert(flat);
    
    if(left) left->optimize2();
    
    if(sib) sib->optimize2();
    
    //    print(stdout); printf("\n");
    
    
    switch(op) {
    case NO_UNKNOWN:
    case NO_INT:
    case NO_VAR:
        break;
        
    case NO_ADD:
    case NO_SUB:
//        code_tree_node * pos = NULL;
//        code_tree_node * neg = NULL;
        code_tree_node * curr = left;
        while(curr) {
            int pn = 1;
            if(curr->op == NO_INT)
                pn = (curr->val > 0)?1:-1;
            curr = curr->sib;
        }
        
        break;
        
    case NO_MUL:
        break;
        
    case NO_DIVC:
    case NO_DIVF:
    case NO_MAX:
    case NO_MIN:
        
        break;
    }


}


void code_tree_node::print(FILE * fp)
{
    char buf[128];
    op_str(buf);
    
    code_tree_node * c;
    if(flat)
        switch(op) {
        case NO_UNKNOWN:
        case NO_INT:
        case NO_VAR:
            fprintf(fp, "%s", buf);
            break;
            
        case NO_ADD:
        case NO_SUB:
        case NO_MUL:
            assert(left);
            left->print(fp);
            c = left->sib;
            while(c) {
                fprintf(fp, "%s", buf);
                c->print(fp);
                c = c->sib;
            }
            break;
            
        case NO_DIVC:
        case NO_DIVF:
        case NO_MAX:
        case NO_MIN:
            assert(left);
            fprintf(fp, "%s(", buf);
            left->print(fp);
            c = left->sib;
            while(c) {
                fprintf(fp, ", ");
                c->print(fp);
                c = c->sib;
            }
            fprintf(fp, ")");
            break;
        }
    else
        switch(op) {
        case NO_UNKNOWN:
        case NO_INT:
        case NO_VAR:
            fprintf(fp, "%s", buf);
            break;
            
        case NO_ADD:
        case NO_SUB:
        case NO_MUL:
            assert(left);
            assert(right);
            left->print(fp);
            fprintf(fp, "%s", buf);
            right->print(fp);
            break;
            
        case NO_DIVC:
        case NO_DIVF:
        case NO_MAX:
        case NO_MIN:
            assert(left);
            assert(right);
            fprintf(fp, "%s(", buf);
            left->print(fp);
            fprintf(fp, ", ");
            right->print(fp);
            fprintf(fp, ")");
            break;
        };

}


void code_tree_node::do_optimize()
{
    flatten();
    optimize1();
    optimize0();
    optimize1();
    optimize0();
}



code_nest * code_nest::make_sib()
{
    code_nest * ns = new code_nest;
    ns->par = this;
    assert(sib == 0);
    sib = ns;
    return ns;
}


code_tree_node * code_nest::create_code_tree(constraint & val, 
                                             char ** name, 
                                             int lev,
                                             node_op cf)
{
    int sign = (val[lev] > 0)?-1:1;
    code_tree_node * Curr = new code_tree_node(val[0]*sign);

    for(int i=1; i<lev; i++)
        if(val[i] != 0) {
            code_tree_node * coif  = new code_tree_node(val[i]*sign);
            code_tree_node * var   = new code_tree_node(name[i-1]);
            code_tree_node * multi = new code_tree_node(NO_MUL, coif, var);
            Curr = new code_tree_node(NO_ADD, Curr, multi);
        }

    if(ABS(val[lev]) != 1) {
        code_tree_node * coif  = new code_tree_node(-1*sign*val[lev]);
        Curr = new code_tree_node(cf, Curr, coif);
    }
    return Curr;
}


code_tree_node * code_nest::create_code_tree(lin_ineq & val, 
                                             char ** name, 
                                             int lev,
                                             node_op mm)
{
    code_tree_node * Curr = NULL;
    node_op cf;
    if(mm == NO_MAX)
        cf = NO_DIVC;
    else
        cf = NO_DIVF;

    for(int i=0; i<val.m(); i++) {
        code_tree_node * a = create_code_tree(val[i], name, lev, cf);
        if(Curr == 0)
            Curr = a;
        else 
            Curr = new code_tree_node(mm, Curr, a);
    }


    return Curr;
}


int code_nest::print(FILE * fp, 
                      int tab)
{
    int curr = 0;
    int tmp;
    print_tab(tab);
    if(eq) {
        fprintf(fp, "%s = ", var);
        lb->print(fp);
        fprintf(fp, ";\n");
        if(sib) {
            tmp = sib->print(fp, tab);
            curr = MAX(tmp, curr);
        }
    } else {
        curr = MAX(curr, 1);
        fprintf(fp, "for(int %s = ", var);
        lb->print(fp);
        fprintf(fp, "; %s <= ", var);
        ub->print(fp);
        if(step == 1) 
            fprintf(fp, "; %s++) ", var);
        else
            fprintf(fp, "; %s += %d)", var, step);
        
        if(sib == 0) {
            fprintf(fp, " {\n");
//            fprintf(fp, " { \n\n");
//            print_tab(tab);
//            fprintf(fp, "}\n");
        } else if(sib->eq) {
            fprintf(fp, " {\n");
            tmp = sib->print(fp, tab+1);
            curr = MAX(tmp, curr);
//            print_tab(tab);
//            fprintf(fp, "}\n");
        } else {
            fprintf(fp, "\n");
            tmp = sib->print(fp, tab+1);
            curr = MAX(tmp, curr);
        }
    }

    return curr;
}


int lin_ineq_op::new_print_code(lin_ineq & a, 
                                 lin_ineq * stride, 
                                 char ** name, 
                                 FILE * fp, 
                                 int pr_acc)
{
    code_nest * root_nest = 0;
    code_nest * curr_nest = 0;
    
    for(int i = 0; i<kind.n(); i++) {
        lin_ineq lo = get_lower(a, i);
        lin_ineq hi = get_upper(a, i);

        if(!hi.is_empty()) {
            
            // All location var's in defining  current location 
            // should be a function of already defined location vars
            int myrank = kind.make_filter(NM_LOCATIONS, i).rank();
            for(int c=0; c< lo.m(); c++)
                if(lo[c].rank() != myrank) lo[c] = 0;
            lo.del_zeros();
            for(c=0; c< hi.m(); c++)
                if(hi[c].rank() != myrank) hi[c] = 0;
            hi.del_zeros();
            
            if(!hi.is_empty()) {
                assert(!lo.is_empty());                         // At least one in each
                if(curr_nest)
                    curr_nest = curr_nest->make_sib();
                else 
                    curr_nest = root_nest = new code_nest;
                
                
                curr_nest->var = new char[strlen(name[lo[0].rank()-1])+1];
                strcpy(curr_nest->var, name[lo[0].rank()-1]);
                
                int single_assign = 0;
                int found_lo, found_hi;
                // Find if var can be expressed as i = .....
                for(found_lo=0; found_lo < lo.m(); found_lo++) {
                    for(found_hi=0; found_hi < hi.m(); found_hi++) {
                        constraint tmp = lo[found_lo] + hi[found_hi];
                        if(tmp.rank() == 0) {                            // Range = 1 
                            if(ABS(tmp[0]) == lo[found_lo][myrank]-1)    // Range is fixed
                                single_assign = 1;
                            
                            // Saman 9/19/92
                            if(ABS(tmp[0]) == 0)                        // Range is i  LB = UB
                                single_assign = 1;
                        }
                        if(single_assign) break;
                    }
                    if(single_assign) break;
                }
                
                //            // BUGBUG:  Make sure no if conditions
                //            if((lo.m() > 1) ||(hi.m() > 1)) single_assign = 0;
                
                if(single_assign) {
                    curr_nest->eq = 1;
                    curr_nest->lb = curr_nest->create_code_tree(lo, name, myrank, NO_MAX);
                    curr_nest->lb->do_optimize();
                } else {
                    curr_nest->lb = curr_nest->create_code_tree(lo, name, myrank, NO_MAX);
                    curr_nest->ub = curr_nest->create_code_tree(hi, name, myrank, NO_MIN);
                    
                    if(stride) {
                        constraint st_filt(a.n());
                        st_filt[myrank] = 1;
                        lin_ineq curr_stride = stride->filter_thru(st_filt, -1);
                        if(curr_stride.m() == 0)
                            curr_nest->step = 1;
                        else {
                            assert(curr_stride.m() == 1);
                            curr_stride[0][myrank] = 0;
                            curr_nest->step = curr_stride[0][curr_stride[0].rank()];
                            curr_stride[0][curr_stride[0].rank()] = 0;
                            assert(curr_stride[0].rank() == 0);
                            
                            /* i == aA + b and  P() <= i
                                a*divceil(P()+b, a) - b <= i  */
                            code_tree_node * b1   = new code_tree_node(curr_stride[0][0]);
                            code_tree_node * P_b  = new code_tree_node(NO_ADD, curr_nest->lb, b1);
                            code_tree_node * a1   = new code_tree_node(curr_nest->step);
                            code_tree_node * divc = new code_tree_node(NO_DIVC, P_b, a1);
                            code_tree_node * a2   = new code_tree_node(curr_nest->step);
                            code_tree_node * mul  = new code_tree_node(NO_MUL, divc, a2);
                            code_tree_node * b2   = new code_tree_node(-1*curr_stride[0][0]);
                            curr_nest->lb = new code_tree_node(NO_ADD, mul, b2);
                            
                        }
                    } else
                        curr_nest->step = 1;
                    
                    curr_nest->lb->do_optimize();
                    curr_nest->ub->do_optimize();
                }
            }
        }        
    }

    if(root_nest)
        return root_nest->print(fp);
    return 0;
}






void lin_ineq::sprint_eq(char * buf, char * names[], int upper, int lhs)
{
    assert(buf);
    strcpy(buf, "");
    char tmpbuf[64];

    for(int i=0; i<m(); i++) {
        if(i != m()-1) 
            strcat(buf, (upper)?"MIN(":"MAX(");

        if(lhs > 0)
            if(ABS((*this)[i][lhs]) != 1) {
                sprintf(tmpbuf, "div%s(", (upper)?"floor":"ceil");
                strcat(buf, tmpbuf);
            }

        int prt = 0;
        if((*this)[i][0] != 0) {
            sprintf(tmpbuf, "%d", (*this)[i][0]);
            strcat(buf, tmpbuf);
            prt = 1;
        }
        for(int j=1; j<n(); j++) 
            if((*this)[i][j] != 0) 
                if(lhs != j) {
                sprintf(tmpbuf, "%d", ABS((*this)[i][j]));
                if((*this)[i][j] == 1) {
                    strcat(buf, (prt)?"+":"");
                    strcat(buf, names[j-1]);
                } else if((*this)[i][j] == -1) {
                    strcat(buf, "-");
                    strcat(buf, names[j-1]);
                } else if((*this)[i][j] > 1) {
                    strcat(buf, (prt)?"+":"");
                    strcat(buf, tmpbuf);
                    strcat(buf, "*");
                    strcat(buf, names[j-1]);
                } else { // (*this)[i][j] < 1
                    strcat(buf, "-");
                    strcat(buf, tmpbuf);
                    strcat(buf, "*");
                    strcat(buf, names[j-1]);
                }
                prt = 1;
            }

        if(prt==0) 
            strcat(buf,"0");

        if(lhs > 0)
            if(ABS((*this)[i][lhs]) != 1) {
                sprintf(tmpbuf, ", %d)", ABS((*this)[i][lhs]));
                strcat(buf, tmpbuf);
            }

        if(i != m()-1) 
            strcat(buf, ", ");
    }
    for(i=0; i<m()-1; i++) 
        strcat(buf, ")");
}
