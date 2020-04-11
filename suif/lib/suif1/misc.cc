/*  Miscellaneous Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#define RCS_BASE_FILE misc_cc

#include "suif1.h"
#include "suif_internal.h"
#include <cstdarg>
#include <csetjmp>
#include <cstring>

RCS_BASE(
    "$Id$")

INCLUDE_SUIF_COPYRIGHT_NAMED(lib_suif)


class clue_data
  {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    clue_data(const clue_data &);
    clue_data &operator=(const clue_data &);

public:
    boolean is_annote;
    union
      {
        annote *the_annote;
        suif_object *the_object;
      } u;

    clue_data(annote *initial_annote) : is_annote(TRUE)
      { u.the_annote = initial_annote; }
    clue_data(suif_object *initial_object): is_annote(FALSE)
      { u.the_object = initial_object; }
  };

clue_data::clue_data(const clue_data &) : is_annote(0)
{ assert(FALSE); }
clue_data &clue_data::operator=(const clue_data &)  
{ assert(FALSE); return *this; } 


DECLARE_DLIST_CLASS(clue_data_list, clue_data *);


/*  global string table */
string_table *lexicon;

/*  target machine parameters */
machine_params target;

boolean ignore_warnings = FALSE;
boolean _suif_no_types = FALSE;
boolean _suif_no_symtabs = FALSE;
boolean _suif_raw_syms = FALSE;
boolean _suif_flat_annotes = FALSE;

char *_suif_program_name;
char *_suif_prog_base_name;
char *_suif_command_line;

boolean show_error_context = FALSE;

static proc_sym *last_diagnostic_proc = NULL;
static clue_data_list *clue_stack = NULL;
static boolean in_context_search = FALSE;
static const char **warnings_seen = NULL;
static unsigned long warning_record_length = 0;

static void announce_proc(proc_sym *the_proc);
static void show_context(void);


/*****************************************************************************/


/*
 *  Miscellaneous utility functions.
 */

int
suif_round_to_int (int n)
{
    return (n + sizeof(int) - 1) / sizeof(int);
}



boolean
is_power_of_2 (int n)
{
    return !(n & (n-1));
}


/*
 *  Make a constant string: In C++ there is no guarantee that a string
 *  passed to a routine will remain a constant since there may be other
 *  pointers to it.  When it is imperative that a string be a constant,
 *  const_string is called to create private copies of the strings.
 */

char *
const_string (const char *s)
{
    return (s == NULL) ? NULL : strcpy(new char[strlen(s) + 1], s);
}


/*
 *  Indent a certain number of spaces.
 */

void
suif_indent (FILE *f, int n)
{
    for (int i = 0; i < n; i++) {
	fprintf(f, "  ");
    }
}


/*
 *  Check if two instructions in a flat list are within the
 *  same expression tree.  This is used for converting to call-by-ref
 *  form and also by the newsuif conversion program.  Search backwards
 *  from the destination instruction to the source instruction to see
 *  if there are any assignments in between.  If so, then the two
 *  instructions are not within the same expression, and the function
 *  returns FALSE.
 */

boolean
within_expr (instruction *dst, instruction *src)
{
    tree_node_list_e *pos = dst->parent()->list_e()->prev();
    while (TRUE) {

	if (!pos || !pos->contents->is_instr()) {
	    assert_msg(FALSE, ("cannot find source instruction %u",
			       src->number()));
	}

	tree_instr *ti = (tree_instr *)pos->contents;
	if (ti->instr() == src) break;

	/* instructions with temporary destinations are allowed */
	operand pdst = ti->instr()->dst_op();
	if (!pdst.is_instr()) return FALSE;

	pos = pos->prev();
    }
    return TRUE;
}


/*****************************************************************************/


const char *__suif_assert_file = NULL;
int __suif_assert_line = -1;
const char *__suif_assert_module = NULL;
jmp_buf * _suif_longjmp_env = NULL;


/*
 *  Assert, with no message.
 */

void
__suif_assert (const char *expr, const char *file, int line, const char *module)
{
    fflush(stdout);
    if (_suif_longjmp_env) longjmp(*_suif_longjmp_env, 1);
    if (in_context_search)
	fprintf(stderr, "\n[assertion failure in context search: ]\n");
    if (_suif_program_name)
	fprintf(stderr, "\n**** Program: %s", _suif_program_name);
    fprintf(stderr, "\n**** Assertion failure in file \"%s\" at line %d",
	    file, line);
    if (module)
	fprintf(stderr, "  (module %s)", module);
    fprintf(stderr, "\n**** False expression: %s\n", expr);
    fflush(stderr);
    show_context();
    abort();
}


/*
 *  Assert, with a message.
 */

void
__suif_assert_msg (const char *msg ...)
{
    va_list ap;
    va_start(ap, msg);

    fflush(stdout);
    if (_suif_longjmp_env) longjmp(*_suif_longjmp_env, 1);
    if (in_context_search)
	fprintf(stderr, "\n[assertion failure in context search: ]\n");
    if (_suif_program_name)
	fprintf(stderr, "\n**** Program: %s", _suif_program_name);
    fprintf(stderr, "\n**** Assertion failure in file \"%s\" at line %d",
	    __suif_assert_file, __suif_assert_line);
    if (__suif_assert_module)
	fprintf(stderr, "  (module %s)", __suif_assert_module);
    fprintf(stderr, "\n**** ");
    vfprintf(stderr, msg, ap);
    va_end(ap);

    putc('\n', stderr);
    fflush(stderr);
    show_context();
    abort();
}


static void find_file_and_line(suif_object *the_object, const char **filename,
                               unsigned *linenum);
static void read_line_annote(immed_list *line_immeds, const char **filename,
                             unsigned *line_num);
static proc_sym *find_enclosing_proc(suif_object *the_object);


/*
 *  This returns the line number in the original source code which generated
 *  the_object, or at least the code in which the_object is now situated, as
 *  best suif can figure it.  This line number is within the file whose name
 *  is returned by source_line_num().  If no line number information can be
 *  found, zero is returned.
 */

unsigned
source_line_num (suif_object *the_object)
{
    unsigned result;
    find_file_and_line(the_object, NULL, &result);
    return result;
}


/*
 *  This returns the name of the original source code file which generated
 *  the_object, or at least the code in which the_object is now situated, as
 *  best suif can figure it.  If the source file information cannot be
 *  found, NULL is returned.
 */

const char *
source_file_name (suif_object *the_object)
{
    const char *result;
    find_file_and_line(the_object, &result, NULL);
    return result;
}


/*
 *  This function finds the last k_line annotation on a tree_node or mark
 *  instruction before or including the tree_node containing the_object on
 *  the same list or any ancestor list.  This is the line annotation that is
 *  currently in effect.  Note that this means that the scope of a k_line
 *  declaration is limited to the rest of the current tree_node_list,
 *  including sub lists of following nodes.  It does not carry to the
 *  remainder of ancestor lists or sibling lists.
 *
 *  If no such k_line annoation can be found, it looks for a
 *  k_source_file_name annotation on the enclosing file_set_entry, if one can
 *  be found.  If none of these is successful, it sets the filename to NULL
 *  and the line to zero.
 */

static void
find_file_and_line (suif_object *the_object, const char **filename,
                    unsigned *linenum)
{
    if (filename != NULL)
        *filename = NULL;
    if (linenum != NULL)
        *linenum = 0;

    if (the_object == NULL)
        return;

    switch (the_object->object_kind()) {
        case FILE_OBJ:
          {
            file_set_entry *the_fse = (file_set_entry *)the_object;
            immed_list *the_immeds =
                    (immed_list *)(the_fse->peek_annote(k_source_file_name));
            if (the_immeds != NULL) {
                assert(the_immeds->count() == 1);
                immed value = the_immeds->head()->contents;
                assert(value.is_string());
                if (filename != NULL)
                    *filename = value.string();
            }
            break;
          }
        case TREE_OBJ:
          {
            tree_node *the_node = (tree_node *)the_object;
            immed_list *the_immeds =
                    (immed_list *)(the_node->peek_annote(k_line));
            if (the_immeds != NULL) {
                read_line_annote(the_immeds, filename, linenum);
                return;
            }
            tree_node_list_e *current_element = the_node->list_e();
            while (current_element != NULL) {
                tree_node *test_node = current_element->contents;
                assert(test_node != NULL);
                immed_list *the_immeds =
                        (immed_list *)(test_node->peek_annote(k_line));
                if (the_immeds != NULL) {
                    read_line_annote(the_immeds, filename, linenum);
                    return;
                }
                if (test_node->kind() == TREE_INSTR) {
                    tree_instr *the_tree_instr = (tree_instr *)test_node;
                    instruction *the_instr = the_tree_instr->instr();
                    if ((the_instr != NULL) &&
                        (the_instr->opcode() == io_mrk)) {
                        immed_list *the_immeds =
                                (immed_list *)(the_instr->peek_annote(k_line));
                        if (the_immeds != NULL) {
                            read_line_annote(the_immeds, filename, linenum);
                            return;
                        }
		    }
		}
                current_element = current_element->prev();
            }

            tree_node_list *parent = the_node->parent();
            if (parent == NULL) {
                if (the_node->is_proc()) {
                    tree_proc *the_proc = (tree_proc *)the_node;
                    find_file_and_line(the_proc->proc()->file(), filename,
                                       linenum);
                }
            } else {
                find_file_and_line(parent->parent(), filename, linenum);
            }
            return;
          }
        case INSTR_OBJ:
          {
            instruction *the_instr = (instruction *)the_object;
            find_file_and_line(the_instr->owner(), filename, linenum);
            break;
          }
        case SYMTAB_OBJ:
          {
            base_symtab *the_symtab = (base_symtab *)the_object;
            if (the_symtab->is_block()) {
                find_file_and_line(((block_symtab *)the_symtab)->block(),
                                   filename, linenum);
            } else if (the_symtab->is_file()) {
                find_file_and_line(((file_symtab *)the_symtab)->fse(),
                                   filename, linenum);
            }
            break;
          }
        case SYM_OBJ:
          {
            sym_node *the_symbol = (sym_node *)the_object;
            find_file_and_line(the_symbol->parent(), filename, linenum);
            break;
          }
        case DEF_OBJ:
          {
            var_def *the_def = (var_def *)the_object;
            find_file_and_line(the_def->parent(), filename, linenum);
            break;
          }
        case TYPE_OBJ:
          {
            type_node *the_type = (type_node *)the_object;
            find_file_and_line(the_type->parent(), filename, linenum);
            break;
          }
        default:
            assert(FALSE);
    }
}


/*
 *  This function finds the enclosing procedure, if any, of the given object.
 */

static proc_sym *find_enclosing_proc(suif_object *the_object)
{
    if (the_object == NULL)
        return NULL;

    switch (the_object->object_kind()) {
        case FILE_OBJ:
            break;
        case TREE_OBJ:
          {
            tree_node *the_node = (tree_node *)the_object;
            return the_node->proc();
          }
        case INSTR_OBJ:
          {
            instruction *the_instr = (instruction *)the_object;
            return find_enclosing_proc(the_instr->owner());
          }
        case SYMTAB_OBJ:
          {
            base_symtab *the_symtab = (base_symtab *)the_object;
            if (the_symtab->is_block()) {
                block_symtab *the_block_symtab = (block_symtab *)the_symtab;
                return find_enclosing_proc(the_block_symtab->block());
            }
            break;
          }
        case SYM_OBJ:
          {
            sym_node *the_symbol = (sym_node *)the_object;
            return find_enclosing_proc(the_symbol->parent());
          }
        case DEF_OBJ:
          {
            var_def *the_def = (var_def *)the_object;
            return find_enclosing_proc(the_def->parent());
          }
        case TYPE_OBJ:
          {
            type_node *the_type = (type_node *)the_object;
            return find_enclosing_proc(the_type->parent());
          }
        default:
            assert(FALSE);
    }
    return NULL;
}


/*
 *  This function reads the contents of the immed list assuming they are in
 *  the form used in a k_line annotation.
 */

static void
read_line_annote(immed_list *line_immeds, const char **filename, unsigned *line_num)
{
    immed_list_iter the_iter(line_immeds);
    if (the_iter.is_empty())
        error_line(1, NULL, "bad format in \"line\" annotation");

    immed value = the_iter.step();
    if (!value.is_unsigned_int())
        error_line(1, NULL, "bad format in \"line\" annotation");

    if (line_num != NULL)
        *line_num = value.unsigned_int();

    if (the_iter.is_empty())
        error_line(1, NULL, "bad format in \"line\" annotation");

    value = the_iter.step();
    if (!value.is_string())
        error_line(1, NULL, "bad format in \"line\" annotation");

    if (filename != NULL)
        *filename = value.string();
}


/*
 *  These two functions print a diagnostic error message to stderr and then
 *  optionally exits or aborts the program.  The diagnostic message printed is
 *  exactly what would be printed by warning_line() except that the word
 *  ``Error'', not ``Warning'', is used.  After that, if the return_code is
 *  zero the function just returns normally.  If the return code is negative,
 *  the program will abort(), which customarily causes a core dump.  If the
 *  return code is positive, the program will exit() with that return code.
 */

void
error_line (int return_code, suif_object *the_object, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    verror_line(return_code, the_object, fmt, ap);
    va_end(ap);
}


void
verror_line (int return_code, suif_object *the_object, const char *fmt, va_list ap)
{
    unsigned src_line;
    const char *src_file_name;
    find_file_and_line(the_object, &src_file_name, &src_line);

    proc_sym *this_proc_sym = find_enclosing_proc(the_object);
    
    fflush(stdout);
    if (_suif_longjmp_env) longjmp(*_suif_longjmp_env, 1);

    if (in_context_search)
	fprintf(stderr, "\n[error in context search: ]\n");

    if ((this_proc_sym != NULL) && (this_proc_sym != last_diagnostic_proc)) {
        last_diagnostic_proc = this_proc_sym;
        announce_proc(this_proc_sym);
    }

    if (src_file_name != NULL) {
        fprintf(stderr, "%s:", src_file_name);
        if (src_line != 0)
            fprintf(stderr, "%u:", src_line);
        fprintf(stderr, "Error");
        if (_suif_prog_base_name != NULL)
            fprintf(stderr, "(%s)", _suif_prog_base_name);
    } else {
        if (_suif_prog_base_name != NULL)
            fprintf(stderr, "%s:", _suif_prog_base_name);
        else
            fprintf(stderr, "%s:", "suif");
        fprintf(stderr, "Error");
    }
    fprintf(stderr, ": ");
    vfprintf(stderr, fmt, ap);
    putc('\n', stderr);
    fflush(stderr);
    show_context();

    if (return_code < 0)
	abort();
    if (return_code > 0)
	exit(return_code);
}


/*
 *  These two functions print a diagnostic warning to stderr in the form
 *
 *      <srcfile>:<srcline>:Warning(<passname>): <message text>
 *
 *  where <srcfile> is the name of the source file and <srcline> the decimal
 *  representation of the line within that file that generated this part of
 *  the suif representation, as nearly as suif can figure it; <passname> is
 *  the name of the program printing the warning; and <message text> is the
 *  text of the warning message as determined by the format string fmt and
 *  optional arguments or argument list, in the manner of printf() or
 *  vprintf() respectively.  The suif_object the_object is used to locate the
 *  error relative to the source code, by way of the functions
 *  source_line_num() and source_file_name().  If the <srcline> cannot be
 *  determined, that and the following colon are omitted from the above
 *  format.  If the passname is not known, it and the surrounding parentheses
 *  are omitted.
 *
 *  If the_object is NULL or the <srcfile> cannot be determined for any other
 *  reason, the diagnostic warning takes the form
 *
 *      <passname>:Warning: <message text>
 *
 *  instead.
 */

void
warning_line (suif_object *the_object, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vwarning_line(the_object, fmt, ap);
    va_end(ap);
}


void
vwarning_line (suif_object *the_object, const char *fmt, va_list ap)
{
    if (ignore_warnings)
        return;

    if (warn_only_once)
      {
        if (warnings_seen == NULL)
          {
            warnings_seen = new const char *[20];
            warning_record_length = 20;
            warnings_seen[0] = NULL;
          }
        else
          {
            const char **follow_warnings = &(warnings_seen[0]);
            while (*follow_warnings != NULL)
              {
                if (*follow_warnings == fmt)
                    return;
                ++follow_warnings;
              }
            if (follow_warnings == &(warnings_seen[warning_record_length - 1]))
              {
                const char **new_block = new const char *[warning_record_length * 2];
                memcpy(new_block, warnings_seen,
                       warning_record_length * sizeof(const char *));
                delete[] warnings_seen;
                warnings_seen = new_block;
                warnings_seen[warning_record_length - 1] = fmt;
                warnings_seen[warning_record_length] = NULL;
                warning_record_length *= 2;
              }
            else
              {
                *follow_warnings = fmt;
                follow_warnings[1] = NULL;
              }
          }
      }

    unsigned src_line;
    const char *src_file_name;
    find_file_and_line(the_object, &src_file_name, &src_line);

    proc_sym *this_proc_sym = find_enclosing_proc(the_object);
    
    fflush(stdout);

    if (in_context_search)
	fprintf(stderr, "\n[warning in context search: ]\n");

    if ((this_proc_sym != NULL) && (this_proc_sym != last_diagnostic_proc)) {
        last_diagnostic_proc = this_proc_sym;
        announce_proc(this_proc_sym);
    }

    if (src_file_name != NULL) {
        fprintf(stderr, "%s:", src_file_name);
        if (src_line != 0)
            fprintf(stderr, "%u:", src_line);
        fprintf(stderr, "Warning");
        if (_suif_prog_base_name != NULL)
            fprintf(stderr, "(%s)", _suif_prog_base_name);
    } else {
        if (_suif_prog_base_name != NULL)
            fprintf(stderr, "%s:", _suif_prog_base_name);
        else
            fprintf(stderr, "%s:", "suif");
        fprintf(stderr, "Warning");
    }
    fprintf(stderr, ": ");
    vfprintf(stderr, fmt, ap);
    putc('\n', stderr);
    fflush(stderr);
    show_context();
}


static void
announce_proc (proc_sym *the_proc)
{
    if ((the_proc == NULL) || (the_proc->name() == NULL))
        return;

    const char *src_file_name = source_file_name(the_proc);
    if (src_file_name != NULL)
        fprintf(stderr, "%s: ", src_file_name);
    fprintf(stderr, "In function `%s':\n", the_proc->name());
}

static void
show_context (void)
{
    if (in_context_search || (!show_error_context))
        return;

    in_context_search = TRUE;

    clue_data_list_e *follow_e = clue_stack->head();
    while (follow_e != NULL)
      {
        clue_data *this_clue = follow_e->contents;
        fprintf(stderr, "[while processing ");
        if (this_clue->is_annote)
          {
            fprintf(stderr, "annotation \"%s\"",
                    this_clue->u.the_annote->name());
          }
        else
          {
            suif_object *this_object = this_clue->u.the_object;
            switch (this_object->object_kind())
              {
                case FILE_OBJ:
                  {
                    file_set_entry *the_fse = (file_set_entry *)this_object;
                    const char *in_name = the_fse->in_name();
                    const char *out_name = the_fse->out_name();
                    fprintf(stderr, "file");
                    if (in_name || out_name)
                        fprintf(stderr, " ");
                    if (in_name)
                        fprintf(stderr, "\"%s\"", in_name);
                    if (in_name && out_name)
                        fprintf(stderr, "/");
                    if (out_name)
                        fprintf(stderr, "\"%s\"", out_name);
                    break;
                  }
                case TREE_OBJ:
                  {
                    tree_node *the_node = (tree_node *)this_object;
                    switch (the_node->kind())
                      {
                        case TREE_INSTR:
                            fprintf(stderr, "tree_instr");
                            break;
                        case TREE_LOOP:
                            fprintf(stderr, "tree_loop");
                            break;
                        case TREE_FOR:
                          {
                            tree_for *the_for = (tree_for *)the_node;
                            fprintf(stderr, "tree_for on `%s'",
                                    the_for->index()->name());
                            break;
                          }
                        case TREE_IF:
                            fprintf(stderr, "tree_if");
                            break;
                        case TREE_BLOCK:
                            fprintf(stderr, "tree_block");
                            break;
                        default:
                            assert(FALSE);
                      }
                    fprintf(stderr, " #%u", the_node->number());
                    break;
                  }
                case INSTR_OBJ:
                  {
                    instruction *the_instr = (instruction *)this_object;
                    fprintf(stderr, "%s instruction #%u",
                            if_ops_name(the_instr->opcode()),
                            the_instr->number());
                    break;
                  }
                case SYMTAB_OBJ:
                  {
                    base_symtab *the_symtab = (base_symtab *)this_object;
                    switch (the_symtab->kind())
                      {
                        case SYMTAB_GLOBAL:
                            fprintf(stderr, "inter-file global");
                            break;
                        case SYMTAB_FILE:
                            fprintf(stderr, "file");
                            break;
                        case SYMTAB_PROC:
                            fprintf(stderr, "procedure");
                            break;
                        case SYMTAB_BLOCK:
                            fprintf(stderr, "block");
                            break;
                        default:
                            assert(FALSE);
                      }
                    fprintf(stderr, " symbol table `%s'", the_symtab->name());
                    break;
                  }
                case SYM_OBJ:
                  {
                    sym_node *the_sym = (sym_node *)this_object;
                    switch (the_sym->kind())
                      {
                        case SYM_PROC:
                            fprintf(stderr, "procedure");
                            break;
                        case SYM_LABEL:
                            fprintf(stderr, "label");
                            break;
                        case SYM_VAR:
                            fprintf(stderr, "variable");
                            break;
                        default:
                            assert(FALSE);
                      }
                    fprintf(stderr, " symbol `%s'", the_sym->name());
                    break;
                  }
                case DEF_OBJ:
                  {
                    var_def *the_def = (var_def *)this_object;
                    fprintf(stderr, "var_def for variable `%s'",
                            the_def->variable()->name());
                    break;
                  }
                case TYPE_OBJ:
                  {
                    type_node *the_type = (type_node *)this_object;
                    switch (the_type->op())
                      {
                        case TYPE_INT:
                            fprintf(stderr, "integer");
                            break;
                        case TYPE_FLOAT:
                            fprintf(stderr, "floating-point");
                            break;
                        case TYPE_VOID:
                            fprintf(stderr, "void");
                            break;
                        case TYPE_PTR:
                            fprintf(stderr, "pointer");
                            break;
                        case TYPE_ARRAY:
                            fprintf(stderr, "array");
                            break;
                        case TYPE_FUNC:
                            fprintf(stderr, "function");
                            break;
                        case TYPE_GROUP:
                            fprintf(stderr, "group");
                            break;
                        case TYPE_STRUCT:
                            fprintf(stderr, "struct");
                            break;
                        case TYPE_UNION:
                            fprintf(stderr, "union");
                            break;
                        case TYPE_ENUM:
                            fprintf(stderr, "enum");
                            break;
                        case TYPE_CONST:
                            fprintf(stderr, "constant");
                            break;
                        case TYPE_VOLATILE:
                            fprintf(stderr, "volatile");
                            break;
			case TYPE_RESTRICT:
			    fprintf(stderr, "restrict");
			    break;
                        case TYPE_CALL_BY_REF:
                            fprintf(stderr, "call-by-reference");
                            break;
                        case TYPE_NULL:
                            fprintf(stderr, "null modifier");
                            break;
                        default:
                            assert(FALSE);
                      }
                    fprintf(stderr, " type #%u (", the_type->type_id());
                    the_type->print_abbrev(stderr);
                    fprintf(stderr, ")");
                    break;
                  }
                default:
                    assert(FALSE);
              }
          }
        fprintf(stderr, "]\n");
        follow_e = follow_e->next();
      }

    fflush(stderr);

    in_context_search = FALSE;
}


void
push_clue (suif_object *the_object)
{
    clue_stack->push(new clue_data(the_object));
}


void
push_clue (annote *the_annote)
{
    clue_stack->push(new clue_data(the_annote));
}


void
pop_clue (suif_object *the_object)
{
    if (clue_stack->is_empty())
        error_line(1, the_object, "pop_clue() called with empty clue stack");
    clue_data *this_clue = clue_stack->pop();
    if (this_clue->is_annote || (this_clue->u.the_object != the_object))
        error_line(1, the_object, "popped clue does not match stack");
    delete this_clue;
}


void
pop_clue (annote *the_annote)
{
    if (clue_stack->is_empty())
        error_line(1, NULL, "pop_clue() called with empty clue stack");
    clue_data *this_clue = clue_stack->pop();
    if ((!this_clue->is_annote) || (this_clue->u.the_annote != the_annote))
        error_line(1, NULL, "popped clue does not match stack");
    delete this_clue;
}


void
init_error_handler (void)
{
    assert(clue_stack == NULL || clue_stack_empty());
    if (clue_stack == NULL)
	clue_stack = new clue_data_list;
}

void exit_error_handler( void ) {
  if( clue_stack != NULL ) {
    delete clue_stack;
    clue_stack = NULL;
  }
}

boolean
clue_stack_empty (void)
{
    return clue_stack->is_empty();
}


/*****************************************************************************/


/*
 *  Procedure iterator.
 */

void
suif_proc_iter (int argc, char * argv[], prociter_f fun, 
                boolean writeback,
                boolean exp_trees,
                boolean use_fortran_form)
{
    if (writeback) {
        if (argc < 2)
            error_line(1, NULL, "No files given");
        if (argc == 2)
            error_line(1, NULL, "No file to write back");
        if (argc % 2 != 1)
            error_line(1, NULL, "File mismatch, file missing to write back");
        for (int i = 1; i < argc; i += 2) {
            fileset->add_file(argv[i], argv[i+1]);
        }
    } else {
        if (argc < 2)
            error_line(1, NULL, "No files given");
        for (int i = 1; i < argc; i++) {
            fileset->add_file(argv[i], NULL);
        }
    }
    
    fileset->reset_iter();
    file_set_entry *fse;
    
    while ((fse = fileset->next_file())) {
        fse->reset_proc_iter();
        
        proc_sym *ps;
        while ((ps = fse->next_proc())) {
            if ((!ps->is_readable()) && (!ps->is_in_memory()))
                continue;

	    if (!ps->is_in_memory()) {
		ps->read_proc(exp_trees,
			      (ps->src_lang() == src_fortran) ?
			      use_fortran_form : FALSE);
	    }

            push_clue(ps->block());
            (fun)(ps->block());
            pop_clue(ps->block());

            if (writeback && !ps->is_written())
                ps->write_proc(fse);
            ps->flush_proc();
	}
    }

    exit_suif1();
}


/*****************************************************************************/


/*
 *  Print out the target machine parameters.
 */

void
machine_params::print (FILE *fp, int depth)
{
    suif_indent(fp, depth);
    fprintf(fp, "Target machine parameters:\n");

    suif_indent(fp, depth+1);
    if (is_big_endian) {
	fprintf(fp, "Big endian\n");
    } else {
	fprintf(fp, "Little endian\n");
    }

    suif_indent(fp, depth+1);
    fprintf(fp, "Size of addressable units = %d\n", addressable_size);

    suif_indent(fp, depth+1);
    if (char_is_signed) {
	fprintf(fp, "Default char type is signed\n");
    } else {
	fprintf(fp, "Default char type is unsigned\n");
    }

    suif_indent(fp, depth+1);
    fprintf(fp, "char type:       size = %d, align = %d\n",
	    size[C_char], align[C_char]);

    suif_indent(fp, depth+1);
    fprintf(fp, "short type:      size = %d, align = %d\n",
	    size[C_short], align[C_short]);

    suif_indent(fp, depth+1);
    fprintf(fp, "int type:        size = %d, align = %d\n",
	    size[C_int], align[C_int]);

    suif_indent(fp, depth+1);
    fprintf(fp, "long type:       size = %d, align = %d\n",
	    size[C_long], align[C_long]);

    suif_indent(fp, depth+1);
    fprintf(fp, "longlong type:   size = %d, align = %d\n",
	    size[C_longlong], align[C_longlong]);

    suif_indent(fp, depth+1);
    fprintf(fp, "float type:      size = %d, align = %d\n",
	    size[C_float], align[C_float]);

    suif_indent(fp, depth+1);
    fprintf(fp, "double type:     size = %d, align = %d\n",
	    size[C_double], align[C_double]);

    suif_indent(fp, depth+1);
    fprintf(fp, "longdouble type: size = %d, align = %d\n",
	    size[C_longdouble], align[C_longdouble]);

    suif_indent(fp, depth+1);
    fprintf(fp, "ptr type:        size = %d, align = %d\n",
	    size[C_ptr], align[C_ptr]);

    suif_indent(fp, depth+1);
    fprintf(fp, "Array alignment = %d\n", array_align);

    suif_indent(fp, depth+1);
    fprintf(fp, "Structure alignment = %d\n", struct_align);

    suif_indent(fp, depth+1);
    fprintf(fp, "Pointer difference type = %d (see C_types enum)\n",
	    ptr_diff_type);
}


boolean
machine_params::operator== (machine_params &other)
{
    for (int type_num = 0; type_num < num_C_types; ++type_num) {
	if (size[type_num] != other.size[type_num])
	    return FALSE;
	if (align[type_num] != other.align[type_num])
	    return FALSE;
    }
    return ((is_big_endian == other.is_big_endian) &&
	    (addressable_size == other.addressable_size) &&
	    (char_is_signed == other.char_is_signed) &&
	    (array_align == other.array_align) &&
	    (struct_align == other.struct_align) &&
	    (ptr_diff_type == other.ptr_diff_type));
}
