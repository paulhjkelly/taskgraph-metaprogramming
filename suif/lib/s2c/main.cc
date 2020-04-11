/* file "main.cc" */

/*  Copyright (c) 1994 Stanford University

All rights reserved.

This software is provided under the terms described in
the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * SUIF to C converter
 *
 * R. French
 * T. Smith
 * C. Wilson
 */

#include "s2c.h"
#include <ctime>
#include <cstring>



int numindent = 0; /* Number of spaces to indent */
boolean in_comment = FALSE;
boolean in_pound_line = FALSE;

boolean log_simp = FALSE;
boolean write_pseudo = FALSE;
boolean keep_casts = FALSE;
boolean omit_header = FALSE;
boolean no_warn = FALSE;
boolean always_intnum = FALSE;
boolean array_exprs = FALSE;
boolean all_annotes_to_comments = FALSE;
boolean gcc_bug_flag = FALSE;
boolean drop_bounds = FALSE;
boolean ll_suffix = FALSE;
boolean explicit_zero_init = FALSE;
boolean limit_escape_sequences = FALSE;
boolean fill_field_space_with_char_arrays = FALSE;
char **show_annote_names = NULL;
boolean show_annote_opcodes[io_last];
boolean show_annote_places[ap_last];

FILE *c_file = stdout;

static label_sym *current_break = NULL;
static label_sym *current_continue = NULL;

const char *k_s2c_label_name_used;
const char *k_s2c_needs_forward_declaration;
const char *k_s2c_comments;
const char *k_s2c_genop_format;
const char *k_s2c_one_use;
const char *k_s2c_multi_use;
const char *k_s2c_init_field;
const char *k_s2c_original_field_names;
const char *k_s2c_pragma;
const char *k_s2c_preamble_pragma;
const char *k_s2c_pound_line;

const char *k_builtin_args;

const char *reserved_words[] =
  {
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "int",
    "long",
    "register",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    NULL
  };

macro_op_data macro_table[] =
  {
    {io_min,      "__suif_min",    "((x)<(y)?(x):(y))", 2, FALSE},
    {io_max,      "__suif_max",    "((x)>(y)?(x):(y))", 2, FALSE},
    {io_abs,      "__suif_abs",    "((x)<0?-(x):(x))",  1, FALSE},
    {io_divfloor, "__suif_divfloor",
     "(((x)<0)^((y)<0) ? ((x) < 0 ? ((x)-(y)+1)/(y) : ((x)-(y)-1)/(y)) : "
     "(x)/(y))", 2, FALSE},
    {io_divceil,  "__suif_divceil",
     "(((x)<0)^((y)<0) ? (x)/(y) : "
     "((x) < 0 ? ((x)+(y)+1)/(y) : ((x)+(y)-1)/(y)))", 2, FALSE},
    {io_rot,      "__suif_rotate", "((x << y) | (x >> (sizeof(x) * %d - y)))", 2,
     FALSE},
    {io_mod,      "__suif_mod",
     "((((x) % (y)) < 0) ? "
     "(((y) < 0) ? (((x) % (y)) - (y)) : (((x) % (y)) + (y))) : ((x) % (y)))",
     2, FALSE},
    {io_mrk,     NULL,       NULL, 0, FALSE}
  };

static const char *place_names[] =
  {
#define place2(x, y) #x,
#define place(x) place2(x, x)
#include "places.h"
#undef place
#undef place2
    NULL
  };

/*
 *  Now set up the output C style parameters.
 */

#include "style_defaults.h"

c_style_type c_style =
  {
    DEFAULT_STYLE_SPACE_AROUND_BINOPS,
    DEFAULT_STYLE_SPACE_AROUND_ASSIGNMENTS,
    DEFAULT_STYLE_SPACE_AFTER_COMMAS,
    DEFAULT_STYLE_NEW_LINE_FOR_OPEN_BRACE,
    DEFAULT_STYLE_NEW_LINE_FOR_ELSE,
    DEFAULT_STYLE_INDENT_SINGLE_STATEMENT_BODIES,
    DEFAULT_STYLE_STATEMENT_INDENT,
    DEFAULT_STYLE_SINGLE_STATEMENT_BODY_INDENT,
    DEFAULT_STYLE_BRACE_INDENT,
    DEFAULT_STYLE_PARAM_INDENT,
    DEFAULT_STYLE_LABEL_INDENT,
    DEFAULT_STYLE_EXTERN_ON_FUNC_DECL,
    DEFAULT_STYLE_MAX_COMMA_ITEMS_PER_LINE
  };


static void parse_arguments(int argc, char *argv[], char **input_filespec,
                            char **output_filespec);
static void usage(void);
static void initialize_macro_table(void);
static void clean_up_macro_table( void );
static void read_global_annotations(global_symtab *the_globals);
static void print_header(void);
static void add_typedecls(ctree *ret, base_symtab *the_symtab);
static void add_vardecls(ctree *ret, base_symtab *the_symtab);
static void add_symtab_annotes(ctree *ret, base_symtab *the_symtab);
static boolean operand_is_intconst(operand the_operand, int *the_const);
static ctree *create_goto(label_sym *the_label);
static ctree *create_branch(label_sym *the_label, ctree *condition);
static ctree *ctree_for_for_index(var_sym *index);
static void print_pound_lines_for_preamble(suif_object *the_object);
static void add_pound_lines_for_object(suif_object *the_object,
                                       ctree *parent_ctree);
static void fix_macro_name(macro_op_data *the_macro);
static boolean is_interfile_name(const char *the_name);
static void fix_relational_op(in_rrr *the_rrr);
static boolean is_null_pointer_const(operand the_op);

static FILE *open_c_file(char *name)
{
    FILE *f = fopen(name, "w");
    if (f == NULL) {
        fprintf(stderr, "Could not open %s for writing.\n", name);
        exit(1);
    }
    return f;
}

static bool registered = false;

extern int s2c_main(int argc, char **argv, file_set_entry *entry, bool toScreen )
{
  //start_suif(argc, argv);
  numindent = 0; /* Number of spaces to indent */
  in_comment = FALSE;
  in_pound_line = FALSE;

  log_simp = FALSE;
  write_pseudo = FALSE;
  keep_casts = FALSE;
  omit_header = FALSE;
  no_warn = FALSE;
  always_intnum = FALSE;
  array_exprs = FALSE;
  all_annotes_to_comments = FALSE;
  gcc_bug_flag = FALSE;
  drop_bounds = FALSE;
  ll_suffix = FALSE;
  explicit_zero_init = FALSE;
  limit_escape_sequences = FALSE;
  fill_field_space_with_char_arrays = FALSE;
  show_annote_names = NULL;

  current_break = NULL;
  current_continue = NULL;

  if (!registered) {
    k_s2c_label_name_used = lexicon->enter("s2c label name used")->sp;
    ANNOTE(k_s2c_needs_forward_declaration, "s2c needs forward declaration",
           FALSE);
    ANNOTE(k_s2c_comments, "s2c comments", TRUE);
    ANNOTE(k_s2c_genop_format, "s2c genop format", TRUE);
    ANNOTE(k_s2c_one_use, "s2c one use", FALSE);
    ANNOTE(k_s2c_multi_use, "s2c multi use", FALSE);
    ANNOTE(k_s2c_init_field, "s2c init field", FALSE);
    ANNOTE(k_s2c_original_field_names, "s2c original field names", FALSE);
    ANNOTE(k_s2c_pragma, "s2c pragma", TRUE);
    ANNOTE(k_s2c_preamble_pragma, "s2c preamble pragma", TRUE);
    ANNOTE(k_s2c_pound_line, "s2c pound line", TRUE);
    ANNOTE(k_builtin_args, "builtin args", TRUE);
    registered=true;
    init_group_layout();
    init_initialization();
  }

    char *input_filespec, *output_filespec;
    parse_arguments(argc, argv, &input_filespec, &output_filespec);

  //assert(input_filespec != NULL);
  //fileset->add_file(input_filespec, NULL);

    if (always_intnum)
      {
        for (int type_num = 0; type_num < num_C_types; ++type_num)
            target.size[type_num] = 0;
        type_signed = fileset->globals()->install_type(new base_type(TYPE_INT,
                                                                     0, TRUE));
      }

  if (toScreen)
    c_file = stdout;
  else if (output_filespec != NULL)
        c_file = open_c_file(output_filespec);

    setbuf(c_file, NULL);

    initialize_macro_table();

    // only one file; no need to loop through --- just get it
  //    fileset->reset_iter();
  //    file_set_entry *fse = fileset->next_file();

    read_global_annotations(fileset->globals());

    pass1_on_symtab(fileset->globals());
  pass1_on_symtab(entry->symtab());

  entry->reset_proc_iter();
    while (TRUE)
      {
      proc_sym *this_proc = entry->next_proc();
        if (this_proc == NULL)
            break;

      //printf("Name:%s\n", this_proc->name() );
      //assert(!this_proc->is_in_memory());
      //this_proc->read_proc();
        pass1_on_proc(this_proc->block());
      //this_proc->flush_proc();
      }

    print_header();

    ctree *globals_tree = new ctree(ctree_semi);

  comment_object(entry);

  if (entry->peek_annote(k_s2c_comments) != NULL)
      {
        ctree *annote_tree = new ctree(ctree_blank_line);
        annote_tree->add_comment(" file set entry annotes: ");
      annote_tree->add_object_comments(entry);
        globals_tree->addchild(annote_tree);
        globals_tree->addchild(new ctree(ctree_blank_line));
      }

    process_globals(globals_tree, fileset->globals());
    print_pound_lines_for_preamble(fileset->globals());
  process_globals(globals_tree, entry->symtab());

  add_pound_lines_for_object(entry, globals_tree);

    file_io the_io(c_file);

    transform_and_print_ctree(&the_io, globals_tree);
    delete globals_tree;

  entry->reset_proc_iter();
    while (TRUE)
      {
      proc_sym *this_proc = entry->next_proc();
        if (this_proc == NULL)
            break;

      //        assert(!this_proc->is_in_memory());
      //        this_proc->read_proc();
        ctree *c = process_proc(this_proc);
        fprintf(c_file, "\n");
        transform_and_print_ctree(&the_io, c);
        delete c;
      //        this_proc->flush_psroc();
      }

  clean_up_macro_table();

    if (!toScreen) 
	fclose(c_file);
  //    exit_suif();
  //    exit(0);
    return 0;
}


static void parse_arguments(int argc, char *argv[], char **input_filespec,
                            char **output_filespec)
{
  char **annote_name_strings = new char *[argc];
  char **annote_opcode_strings = new char *[argc];
  char **annote_place_strings = new char *[argc];
    cmd_line_option option_table[] =
      {
        {CLO_NOARG,        "-logic-simp",          NULL, &log_simp},
        {CLO_NOARG,        "-pseudo",              NULL, &write_pseudo},
        {CLO_NOARG,        "-keep-casts",          NULL, &keep_casts},
        {CLO_NOARG,        "-omit-header",         NULL, &omit_header},
        {CLO_NOARG,        "-no-warn",             NULL, &no_warn},
        {CLO_NOARG,        "-always-intnum",       NULL, &always_intnum},
        {CLO_NOARG,        "-array-exprs",         NULL, &array_exprs},
        {CLO_NOARG,        "-annotes-all",         NULL,
                 &all_annotes_to_comments},
        {CLO_MULTI_STRING, "-annotes-named",       NULL,
                 &annote_name_strings},
        {CLO_MULTI_STRING, "-annotes-opcode",      NULL,
                 &annote_opcode_strings},
        {CLO_MULTI_STRING, "-annotes-object-kind", NULL,
                 &annote_place_strings},
        {CLO_NOARG,        "-gcc-bug",             NULL, &gcc_bug_flag},
        {CLO_NOARG,        "-drop-bounds",         NULL, &drop_bounds},
        {CLO_NOARG,        "-ll-suffix",           NULL, &ll_suffix},
        {CLO_NOARG,        "-explicit-zero-init",  NULL, &explicit_zero_init},
        {CLO_NOARG,        "-limit-escape-sequences", NULL,
                 &limit_escape_sequences},
        {CLO_NOARG,        "-fill-field-space-with-char-arrays", NULL,
                 &fill_field_space_with_char_arrays}
      };

    assert((input_filespec != NULL) && (output_filespec != NULL));

    parse_cmd_line(argc, argv, option_table,
                   sizeof(option_table) / sizeof(cmd_line_option));

    if (annote_name_strings[0] != NULL)
      {
        int num_strings = 0;
        while (annote_name_strings[num_strings] != NULL)
            ++num_strings;
        show_annote_names = new char *[num_strings + 1];
        for (int string_num = 0; string_num < num_strings; ++string_num)
            show_annote_names[string_num] = annote_name_strings[string_num];
        show_annote_names[num_strings] = NULL;
      }

    int string_num;
    for (string_num = 0; annote_opcode_strings[string_num] != NULL;
         ++string_num)
      {
        char *this_name = annote_opcode_strings[string_num];
        int opcode;
        for (opcode = 0; opcode < io_last; ++opcode)
          {
            if (strcmp(if_ops_name((enum if_ops)opcode), this_name) == 0)
                break;
          }
        if (opcode == io_last)
          {
            error_line(1, NULL, "\"%s\" is not a valid opcode name",
                       this_name);
          }
        show_annote_opcodes[opcode] = TRUE;
      }

    for (string_num = 0; annote_place_strings[string_num] != NULL;
         ++string_num)
      {
        char *this_name = annote_place_strings[string_num];
        int place_num;
        for (place_num = 0; place_num < ap_last; ++place_num)
          {
            if (strcmp(place_names[place_num], this_name) == 0)
                break;
          }
        if (place_num == ap_last)
          {
            error_line(1, NULL, "\"%s\" is not a valid object kind",
                       this_name);
          }
          else
          {
            show_annote_places[place_num] = TRUE;
          }

        if (place_num == ap_nodes)
          {
            show_annote_places[ap_loops] = TRUE;
            show_annote_places[ap_fors] = TRUE;
            show_annote_places[ap_ifs] = TRUE;
            show_annote_places[ap_blocks] = TRUE;
          }
        if (place_num == ap_all_syms)
          {
            show_annote_places[ap_vars] = TRUE;
            show_annote_places[ap_proc_syms] = TRUE;
            show_annote_places[ap_labels] = TRUE;
          }
      }

  delete [] annote_name_strings;
  delete [] annote_opcode_strings;
  delete [] annote_place_strings;

    if (argc < 2)
      {
        fprintf(stderr, "No input SUIF file specified.\n");
        usage();
        exit(1);
      }

    if (argc > 3)
      {
        fprintf(stderr, "Too many files specified.\n");
        usage();
        exit(1);
      }

    *input_filespec = argv[1];
    if (argc == 3)
        *output_filespec = argv[2];
    else
        *output_filespec = NULL;
}


static void usage(void)
{
    const char *usage_message[] = {

    "usage: s2c [options] SUIF-file [c-file]\n",
    "\n",
    "  options:\n",
    "    -logic-simp\n",
    "        do simple logical simplification\n",
    "    -pseudo\n",
    "        write pseudo-C if correct C impossible\n",
    "    -keep-casts\n",
    "        keep all SUIF type casts (by default they are folded away when C\n",
    "        would put in implicit casts)\n",
    "    -omit-header\n",
    "        do not write header comments\n",
    "    -no-warn\n",
    "        do not issue any warnings\n",
    "    -always-intnum\n",
    "        never use C type names for integers\n",
    "    -array-exprs\n",
    "        use illegal array expressions\n",
    "    -annotes-all\n",
    "        write all annotations as comments\n",
    "    -annotes-named <name>\n",
    "        write all annotations with the given name\n",
    "    -annotes-opcode <opcode>\n",
    "        write all annotations on instructions with the given opcode\n",
    "    -annotes-object-kind <kind>\n",
    "        write all the annotations on one specific kind of object, where\n",
    "        <kind> is one of the following:\n",
    "              fses           all file set entries\n",
    "              nodes          all tree nodes (except tree_instr's)\n",
    "                loops        all ``loop'' nodes\n",
    "                fors         all ``for'' nodes\n",
    "                ifs          all ``if'' nodes\n",
    "                blocks       all ``block'' nodes\n",
    "              all-instrs     all instructions\n",
    "              symtabs        all symtabs\n",
    "              all-syms       all symbols\n",
    "                vars         all variable symbols\n",
    "                proc-syms    all procedure symbols\n",
    "                labels       all label symbols\n",
    "              var-defs       all variable definitions\n",
    "              types          all types\n",
    "    -gcc-bug\n",
    "        use a work-around for a gcc bug in initialization of unnamed\n"
    "        bit-fields\n"
    "    -drop-bounds\n",
    "        make for bounds temporaries if they contain loads, uses of\n",
    "        symbols with their addresses possibly taken, including\n",
    "        globals, or SUIF intrinsics that might be turned into\n",
    "        control-flow, such as io_max; the idea is to make sure it is\n",
    "        clear to the back-end C compiler that the bounds are loop\n",
    "        invariant, so loop optimizations such as software pipeling may\n",
    "        be done\n",
    "    -ll-suffix\n",
    "        use ``ll'' and ``ull'' suffixes on integer constants of\n",
    "        ``long long'' and ``unsigned long long'' type respectively\n",
    "    -explicit-zero-init\n",
    "        make zero initializations of static data explicit; by default,\n",
    "        such initializations are left implicit\n",
    "    -limit-escape-sequences\n",
    "        do not produce simple alphabetic escape sequences other than\n",
    "        \\n (i.e. no \\a, \\b, \\f, \\r, \\t, or \\v); this is a "
    "work-around\n",
    "        for some back-end compilers that don't recognize all the ANSI\n",
    "        C required alphabetic escape sequences (the DEC OSF 3.2 cc\n",
    "        compiler, for example, doesn't recognize \\a)\n",
    "    -fill-field-space-with-char-arrays\n",
    "        use character arrays to pad structures instead of bit fields,\n",
    "        when possible; this is a work-around for back-end compilers\n",
    "        that lay out bit fields differently\n"};

    for (size_t line_num = 0;
         line_num < sizeof(usage_message) / sizeof(char *); ++line_num)
      {
        fprintf(stderr, "%s", usage_message[line_num]);
      }
}



/*
 * Initialize the macro table.  Most of this was done statically, but the
 * expansion of the ``rotate'' macro needs to use the size in bits of an
 * addressable unit in the target machine, which is read in from the SUIF file
 * by the library.
 */
static void initialize_macro_table(void)
{
    for (int index = 0; macro_table[index].replacement != NULL; ++index)
      {
        if (macro_table[index].opcode == io_rot)
          {
            char *new_replacement =
                    new char[strlen(macro_table[index].replacement) + 20];
            sprintf(new_replacement, macro_table[index].replacement,
                    target.addressable_size);
            macro_table[index].replacement = new_replacement;
          }
      }
}

static void clean_up_macro_table( void ) {
  for (int index = 0; macro_table[index].replacement != NULL; ++index) {
    if( macro_table[index].opcode == io_rot ) {
      if( macro_table[index].replacement != NULL ) {
	delete [] macro_table[index].replacement;
	macro_table[index].replacement = NULL;
      }
    }
  }
}

static void read_global_annotations(global_symtab *the_globals)
{
    annote_list_iter the_iter(the_globals->annotes());
    while (!the_iter.is_empty())
      {
        annote *this_annote = the_iter.step();
        if (this_annote->name() == k_s2c_genop_format)
          {
            immed_list *format_immeds = this_annote->immeds();
            boolean matched = FALSE;
            if (format_immeds->count() == 2)
              {
                immed first_data = (*format_immeds)[0];
                immed second_data = (*format_immeds)[1];
                if (first_data.is_string() && second_data.is_string())
                  {
                    register_gen_op(first_data.string(), second_data.string(),
                                    TRUE, 0);
                    matched = TRUE;
                  }
              }
            else if (format_immeds->count() == 3)
              {
                immed first_data = (*format_immeds)[0];
                immed second_data = (*format_immeds)[1];
                immed third_data = (*format_immeds)[2];
                if (first_data.is_string() && second_data.is_integer() &&
                    third_data.is_string())
                  {
                    register_gen_op(first_data.string(), third_data.string(),
                                    FALSE, second_data.integer());
                    matched = TRUE;
                  }
              }

            if (!matched)
              {
                error_line(1, NULL, "bad format for \"%s\" annotation",
                           k_s2c_genop_format);
              }
          }
      }
}

static void print_header(void)
{
  omit_header = true;
    if (!omit_header)
      {
        time_t systime;

        fprintf(c_file, "/*\n");
        fprintf(c_file, " * This file was created automatically from SUIF\n");
        systime = time(NULL);
        char *time_string = ctime(&systime);
        char *new_time_string = new char[strlen(time_string)];
        strncpy(new_time_string, time_string, strlen(time_string));
        new_time_string[strlen(time_string) - 1] = 0;
        fprintf(c_file, " *   on %s.\n", new_time_string);
        delete[] new_time_string;
        fprintf(c_file, " *\n");
        fprintf(c_file, " * Created by:\n");
        fprintf(c_file, " * %s %s %s\n", _suif_prog_base_name, prog_ver_string,
                prog_who_string);
        fprintf(c_file, " *     Based on SUIF distribution %s\n",
                libsuif_suif_string);
        fprintf(c_file, " *     Linked with:\n");

        library_list_iter library_iter(suif_libraries);
        while (!library_iter.is_empty())
          {
            suif_library *this_library = library_iter.step();
            fprintf(c_file, " *   lib%s %s %s\n", this_library->name,
                    this_library->version, this_library->compile_info);
          }

        fprintf(c_file, " */\n\n\n");
      }

    boolean definition = FALSE;
    for (int index = 0; macro_table[index].replacement != NULL; ++index)
      {
        if (macro_table[index].used)
          {
            fix_macro_name(&(macro_table[index]));
            fprintf(c_file, "#define %s(x", macro_table[index].op_name);
            if (macro_table[index].num_operands == 2)
                fprintf(c_file, ",y");
            else
                assert(macro_table[index].num_operands == 1);
            fprintf(c_file, ") %s\n", macro_table[index].replacement);
            definition = TRUE;
          }
      }

    if (definition)
        fprintf(c_file, "\n");
}



/*
 * Add globals.
 */

void process_globals(ctree *ret, global_symtab *syms)
{
    layout_groups_for_symtab(syms);
    preprocess_symtab(syms);

    add_typedecls(ret, syms);
    add_vardecls(ret, syms);
    add_symtab_annotes(ret, syms);
    add_pound_lines_for_object(syms, ret);
}




/*
 * Add parameters to a funcdecl ctree.
 */

extern ctree *process_params(proc_sym *psym)
{
    ctree *func = new ctree(ctree_funcdef, psym);

    sym_node_list_iter snli(psym->block()->proc_syms()->params());
    while (!snli.is_empty()) {
        sym_node *sym = snli.step();
        assert(sym->is_var());
        var_sym *vs = (var_sym*)sym;
        assert(vs->is_param());
        ctree *var = new ctree(ctree_vardecl, vs);
        func->addchild(var);
    }

    return func;
}

extern void transform_and_print_ctree(io_class *out, ctree *the_ctree)
{
    /* The order here is important! */
    the_ctree->extract_data();
    the_ctree->fold_converts();
    the_ctree->do_basic_folding();
    the_ctree->flatten();

    if (log_simp) {
        the_ctree->cond_simp();
        the_ctree->log_simp();
    }

    the_ctree->truncate_zeros_in_static_inits();

    the_ctree->print_as_c(out, 0);
}

/*
 * Process a procedure from a TREE into a CTREE.
 */

ctree *process_proc(proc_sym *psym)
{
    preprocess_proc(psym->block());
    preprocess_symtab(psym->block()->symtab());

    ctree *func_tree = process_params(psym);
    ctree *main_tree = new ctree(ctree_semi);

    /*
     * Add local variable declarations to the C tree.
     */

    add_typedecls(main_tree, psym->block()->symtab());
    add_vardecls(main_tree, psym->block()->symtab());
    add_symtab_annotes(main_tree, psym->block()->symtab());
    add_pound_lines_for_object(psym->block()->symtab(), main_tree);

    /*
     * Generate a C tree for the instruction list.
     */

    main_tree->addchild(process_node_list(psym->block()->body(), NULL, NULL));

    func_tree->addchild(main_tree);
    return func_tree;
}

/*
 * Generate a C tree for a list of tree nodes
 */

extern ctree *process_node_list(tree_node_list *nl, label_sym *target,
                                ctree **conditional_expr)
{
    ctree *ret = new ctree(ctree_semi);
    ctree *tree;

    ctree *pending_expr = NULL;
    label_sym *pending_target = NULL;

    tree_node_list_iter tnli(nl);
    while (!tnli.is_empty()) {
        tree_node *node = tnli.step();
        tree = NULL;

        if ((pending_expr != NULL) && (!node->is_instr()))
          {
            ctree *branch_tree = create_branch(pending_target, pending_expr);
            ret->addchild(branch_tree);

            pending_expr = NULL;
            pending_target = NULL;
          }

        switch (node->kind()) {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)node;
            ctree *pre_instr = NULL;
            tree = process_base_inst(the_tree_instr->instr(), &pending_expr,
                                     &pending_target, &pre_instr);
            if (pre_instr != NULL)
                ret->addchild(pre_instr);
            break;
          }
        case TREE_LOOP:
            tree = process_loop((tree_loop*)node);
            break;
        case TREE_FOR:
            tree = process_for((tree_for*)node);
            break;
        case TREE_IF:
            tree = process_if((tree_if*)node);
            break;
        case TREE_BLOCK:
            tree = process_block((tree_block*)node);
            break;
        }
        if (tree)
            ret->addchild(tree);
    }

    if (pending_expr != NULL)
      {
        if ((target == pending_target) && (conditional_expr != NULL))
          {
            *conditional_expr = pending_expr;
          }
        else
          {
            ctree *branch_tree = create_branch(pending_target, pending_expr);
            ret->addchild(branch_tree);
          }
      }

    return ret;
}

extern ctree *process_solo_instr(instruction *the_instr)
{
    ctree *semi_tree = new ctree(ctree_semi);
    ctree *pending_expr = NULL;
    label_sym *pending_target = NULL;
    ctree *pre_instr = NULL;
    ctree *base_tree =
            process_base_inst(the_instr, &pending_expr, &pending_target,
                              &pre_instr);
    if (pre_instr != NULL)
        semi_tree->addchild(pre_instr);
    if (base_tree != NULL)
        semi_tree->addchild(base_tree);
    if (pending_expr != NULL)
        semi_tree->addchild(create_branch(pending_target, pending_expr));

    if (semi_tree->N() == 1)
      {
        ctree *child_tree = semi_tree->child(0);
        semi_tree->reset_children();
        child_tree->copy_comments_from(semi_tree);
        delete semi_tree;
        return child_tree;
      }
    return semi_tree;
}

/*
 * Generate a C tree for a statement of some sort
 */

ctree *operand_to_tree(const operand &op)
{
    switch (op.kind()) {
    case OPER_SYM:
        return new ctree(ctree_symconst, op.symbol(), op.symbol()->type());
    case OPER_INSTR:
      {
        ctree *result = process_expr_inst(op.instr());
        result->add_object_comments(op.instr());
        return result;
      }
    case OPER_NULL:
      {
        static boolean warned = FALSE;
        mistake(&warned, NULL, "illegal null operand");
        return new ctree(ctree_macro, "<null>", type_signed);
      }
    default:
        assert_msg(FALSE, ("operand_to_tree - Illegal operand type"));
    }
    return NULL;
}

extern ctree *process_base_inst(instruction *inst, ctree **pending_expr,
                                label_sym **pending_target, ctree **pre_instr)
{
    ctree *tree;

    if_ops op = inst->opcode();

    if ((op == io_mrk) || (op == io_nop))
      {
        ctree *result = NULL;
        if ((inst->peek_annote(k_s2c_pragma) != NULL) ||
            (inst->peek_annote(k_s2c_pound_line) != NULL))
          {
            result = new ctree(ctree_semi);
            add_pound_lines_for_object(inst, result);
          }
        if (inst->peek_annote(k_s2c_comments) != NULL)
          {
            ctree *new_ctree = new ctree(ctree_blank_line);
            new_ctree->add_object_comments(inst);
            if (result == NULL)
                result = new_ctree;
            else
                result->addchild(new_ctree);
          }
        return result;
      }

    if (which_format(op) == inf_bj)
      {
        in_bj *the_bj = (in_bj *)inst;

        if ((pending_expr != NULL) && (pending_target != NULL))
          {
            ctree *new_expr;
            if (op == io_jmp)
              {
                new_expr = ldc_tree(type_signed, immed(1));
              }
            else
              {
                new_expr = operand_to_tree(the_bj->src_op());
                if (op == io_bfalse)
                  {
                    ctree *not_expr = new ctree(ctree_not);
                    not_expr->addchild(new_expr);
                    new_expr = not_expr;
                  }
              }

            new_expr->add_object_comments(the_bj);

            if ((*pending_expr != NULL) &&
                (*pending_target == the_bj->target()))
              {
                ctree *or_expr = new ctree(ctree_logor);
                or_expr->addchild(*pending_expr);
                or_expr->addchild(new_expr);

                *pending_expr = or_expr;
                return NULL;
              }

            ctree *result = NULL;
            if (*pending_expr != NULL)
                result = create_branch(*pending_target, *pending_expr);

            *pending_expr = new_expr;
            *pending_target = the_bj->target();
            return result;
          }

        ctree *the_goto = create_goto(the_bj->target());
        the_goto->add_object_comments(the_bj);

        if (op == io_jmp)
          {
            return the_goto;
          }
        else
          {
            ctree *gif = new ctree(ctree_if);
            ctree *test_ctree = operand_to_tree(the_bj->src_op());
            if (op == io_bfalse)
              {
                ctree *new_test_ctree = new ctree(ctree_not);
                new_test_ctree->addchild(test_ctree);
                test_ctree = new_test_ctree;
              }
            gif->addchild(test_ctree);
            gif->addchild(the_goto);
            return gif;
          }
      }

    if ((pending_expr != NULL) && (pending_target != NULL))
      {
        if (*pending_expr != NULL)
          {
            assert(pre_instr != NULL);

            *pre_instr = create_branch(*pending_target, *pending_expr);

            *pending_expr = NULL;
            *pending_target = NULL;
          }
      }

    switch (which_format(op)) {
    case inf_rrr: {
        in_rrr *rrr = (in_rrr *) inst;
        if (op == io_str || op == io_memcpy) {
            tree = new ctree(ctree_assign);
            ctree *indtree = new ctree(ctree_deref);
            indtree->addchild(operand_to_tree(rrr->src1_op()));
            tree->addchild(indtree);
            if (op == io_str)
                tree->addchild(operand_to_tree(rrr->src2_op()));
            else {
                ctree *deref_tree = new ctree(ctree_deref);
                deref_tree->addchild(operand_to_tree(rrr->src2_op()));
                tree->addchild(deref_tree);
            }
            tree->add_object_comments(rrr);
            return tree;
        } else if (op == io_ret) {
            tree = new ctree(ctree_return);
            if (!rrr->src1_op().is_null())
                tree->addchild(operand_to_tree(rrr->src1_op()));
            tree->add_object_comments(rrr);
            return tree;
        }
        break;
    }
    case inf_lab:
      {
        in_lab *lab = (in_lab*)inst;
        ctree *result_ctree = new ctree(ctree_label, lab->label());
        result_ctree->add_object_comments(lab);
        return result_ctree;
      }
    case inf_mbr:
      {
        in_mbr *the_mbr = (in_mbr *)inst;

        ctree *semi_tree = new ctree(ctree_semi);

        unsigned num_labs = the_mbr->num_labs();
        for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
          {
            ctree *case_tree =
                    new ctree(ctree_case, NULL,
                              i_integer(lab_num) + the_mbr->lower());
            semi_tree->addchild(case_tree);

            ctree *goto_tree = create_goto(the_mbr->label(lab_num));
            semi_tree->addchild(goto_tree);
          }

        if (the_mbr->default_lab() != NULL)
          {
            ctree *case_tree = new ctree(ctree_default);
            semi_tree->addchild(case_tree);

            ctree *goto_tree = create_goto(the_mbr->default_lab());
            semi_tree->addchild(goto_tree);
          }

        ctree *block_tree = new ctree(ctree_block);
        block_tree->addchild(semi_tree);

        ctree *switch_tree = new ctree(ctree_switch);
        switch_tree->addchild(operand_to_tree(the_mbr->src_op()));
        switch_tree->addchild(block_tree);

        switch_tree->add_object_comments(the_mbr);

        return switch_tree;
      }
    default:
        break;
    }

    tree = process_expr_inst(inst);
    tree->add_object_comments(inst);

    if (inst->dst_op().is_symbol())
      {
        ctree *assgn = new ctree(ctree_assign);
        assgn->addchild(operand_to_tree(inst->dst_op()));
        assgn->addchild(tree);
        return assgn;
      }
    return tree;
}


extern ctree *process_expr_inst(instruction *inst)
{
  ctree *tree = NULL;

    if_ops op = inst->opcode();

    assert(op != io_nop);

    switch (which_format(op)) {
    case inf_rrr: {
        in_rrr *rrr = (in_rrr *) inst;
        if (op == io_cvt) {
            ctree *operand_tree = operand_to_tree(rrr->src_op());

            immed_list *field_immeds =
                    (immed_list *)(rrr->peek_annote(k_fields));
            if (field_immeds != NULL)
              {
                type_node *src_type = rrr->src_op().type();
                if (!src_type->unqual()->is_ptr())
                  {
                    error_line(1, rrr->parent(),
                               "\"%s\" annotation on convert from non-pointer"
                               " type", k_fields);
                  }

                ptr_type *src_ptr = (ptr_type *)(src_type->unqual());
                type_node *object_type = src_ptr->ref_type();

                ctree *object_tree = new ctree(ctree_deref);
                object_tree->addchild(operand_tree);

                tree = get_address_with_offset(object_tree, object_type,
                                               rrr->result_type(), 0,
                                               field_immeds);
              }
            else
              {
                tree = new ctree(ctree_conv, !keep_casts, rrr->result_type());
                tree->addchild(operand_tree);
              }

            return tree;
        } else {
            if (((op == io_add) || (op == io_sub)) &&
                rrr->result_type()->unqual()->is_ptr())
              {
                operand ptr_op = rrr->src1_op();
                operand int_op = rrr->src2_op();

                if ((op == io_add) && !ptr_op.type()->unqual()->is_ptr())
                  {
                    operand temp_op = ptr_op;
                    ptr_op = int_op;
                    int_op = temp_op;
                  }

                if ((!ptr_op.type()->unqual()->is_ptr()) ||
                    (int_op.type()->unqual()->op() != TYPE_INT))
                  {
                    error_line(1, rrr->parent(), "illegal pointer arithmetic");
                  }

                ptr_type *src_ptr_type = (ptr_type *)(ptr_op.type()->unqual());

                immed_list *field_immeds =
                        (immed_list *)(rrr->peek_annote(k_fields));
                if ((op == io_add) && (field_immeds != NULL))
                  {
                    int offset = 0;
                    boolean is_const = operand_is_intconst(int_op, &offset);
                    if (!is_const)
                      {
                        error_line(1, rrr->parent(),
                                   "\"%s\" annotation on addition of "
                                   "non-constant", k_fields);
                      }
                    offset *= target.addressable_size;

                    type_node *object_type = src_ptr_type->ref_type();

                    ctree *object_tree = new ctree(ctree_deref);
                    object_tree->addchild(operand_to_tree(ptr_op));

                    tree = get_address_with_offset(object_tree, object_type,
                                                   rrr->result_type(), offset,
                                                   field_immeds);

                    return tree;
                  }

                if (src_ptr_type->ref_type()->size() !=
                    target.addressable_size)
                  {
                    /*
                     * In this case, the units SUIF and C use for pointer
                     * arithmetic are different.  SUIF always uses bytes while
                     * C uses the size of the thing pointed to.
                     */
                    int factor =
                            src_ptr_type->ref_type()->size() /
                            target.addressable_size;
                    ctree *int_ctree = operand_to_tree(int_op);
                    if (factor != 0)
                      {
                        boolean folded = FALSE;
                        int_ctree->try_const_div(factor, &folded);
                        if (folded)
                          {
                            tree = new ctree(ctree::ctree_io_to_op(op));
                            tree->addchild(operand_to_tree(ptr_op));
                            tree->addchild(int_ctree);
                            return force_type(tree, rrr->result_type());
                          }
                      }

                    ctree *src_conv_tree = operand_to_tree(ptr_op);
                    src_conv_tree =
                            force_type(src_conv_tree, type_char->ptr_to());

                    ctree *add_tree = new ctree(ctree::ctree_io_to_op(op));
                    add_tree->addchild(src_conv_tree);
                    add_tree->addchild(int_ctree);

                    return force_type(add_tree, rrr->result_type());
                  }
              }
            else if ((op == io_sub) &&
                     rrr->src1_op().type()->unqual()->is_ptr())
              {
                type_node *src1_type = rrr->src1_op().type()->unqual();
                type_node *src2_type = rrr->src2_op().type()->unqual();

                if ((!src1_type->is_ptr()) || (!src2_type->is_ptr()) ||
                    (rrr->result_type()->unqual()->op() != TYPE_INT))
                  {
                    error_line(1, rrr->parent(), "illegal pointer arithmetic");
                  }

                ptr_type *src1_ptr = (ptr_type *)src1_type;

                ctree *src1_tree = operand_to_tree(rrr->src1_op());
                ctree *src2_tree = operand_to_tree(rrr->src2_op());
                src2_tree = force_type(src2_tree, src1_ptr);

                if (src1_ptr->ref_type()->size() != target.addressable_size)
                  {
                    /*
                     * In this case, the units SUIF and C use for pointer
                     * arithmetic are different.  SUIF always uses bytes while
                     * C uses the size of the thing pointed to.
                     */
                    int factor =
                            src1_ptr->ref_type()->size() /
                            target.addressable_size;

                    if (factor != 0)
                      {
                        ctree *add_tree = new ctree(ctree::ctree_io_to_op(op));
                        add_tree->addchild(src1_tree);
                        add_tree->addchild(src2_tree);

                        ctree *const_tree =
                                ldc_tree(rrr->result_type(), immed(factor));

                        tree = new ctree(ctree_mult);
                        tree->addchild(add_tree);
                        tree->addchild(const_tree);
                      }
                    else
                      {
                        ctree *src1_conv =
                                force_type(src1_tree, type_char->ptr_to());
                        ctree *src2_conv =
                                force_type(src2_tree, type_char->ptr_to());

                        tree = new ctree(ctree::ctree_io_to_op(op));
                        tree->addchild(src1_conv);
                        tree->addchild(src2_conv);
                      }
                  }
                else
                  {
                    tree = new ctree(ctree::ctree_io_to_op(op));
                    tree->addchild(src1_tree);
                    tree->addchild(src2_tree);
                  }

                return force_type(tree, rrr->result_type());
              }

            if (op == io_cpy)
              {
                return force_type(operand_to_tree(rrr->src_op()),
                                  rrr->result_type());
              }

            int index;
            for (index = 0; macro_table[index].replacement != NULL; ++index)
              {
                if (macro_table[index].opcode == op)
                  {
                    tree = new ctree(ctree_macro, macro_table[index].op_name);
                    assert(macro_table[index].used || in_comment ||
                           in_pound_line);
                    break;
                  }
              }

            if (macro_table[index].replacement == NULL)
                tree = new ctree(ctree::ctree_io_to_op(op));

            fix_relational_op(rrr);

            tree->addchild(operand_to_tree(rrr->src1_op()));
            if (!rrr->src2_op().is_null())
                tree->addchild(operand_to_tree(rrr->src2_op()));

            return force_type(tree, rrr->result_type());
        }
    }
    case inf_ldc:
      {
        in_ldc *the_ldc = (in_ldc *)inst;
        immed_list *field_immeds =
                (immed_list *)(the_ldc->peek_annote(k_fields));
        tree = ldc_tree(the_ldc->result_type(), the_ldc->value(),
                        field_immeds);

        return tree;
      }
    case inf_array:
      {
        in_array *ar = (in_array *)inst;
        ctree *artree = new ctree(ctree_subscr);
        ctree *base_tree = operand_to_tree(ar->base_op());

        /*
         *  In SUIF, the type of the base operand of an array instruction
         *  should be a pointer to an array of the element type.  In C it
         *  should be either a pointer to or an array of the element type
         *  directly.  If C were to use the SUIF type, it would do the
         *  arithmetic wrong, using the size of the whole array (which might
         *  not even be known) as the unit of the index.
         *  We'll put in the cast here, though it may be optimized away by
         *  another part of the program if C would do an implicit conversion
         *  anyway.
         */
        type_node *base_type = ar->base_op().type()->unqual();
        if (!base_type->is_ptr())
          {
            error_line(1, inst->parent(),
                       "base of array reference is not a pointer");
          }
        ptr_type *base_ptr = (ptr_type *)base_type;
        type_node *base_ref = base_ptr->ref_type()->unqual();
        if (!base_ref->is_array())
          {
            error_line(1, inst->parent(),
                       "base of array reference is not a pointer to an array");
          }
        array_type *base_array = (array_type *)base_ref;
        boolean flatten = array_flattening_needed(ar, base_array);
        type_node *element_type = base_array->elem_type();

        if (flatten)
          {
            for (unsigned dim_num = ar->dims(); dim_num > 1; --dim_num)
              {
                if (!element_type->unqual()->is_array())
                  {
                    error_line(1, ar->parent(),
                               "array type mismatches array reference "
                               "instruction");
                  }
                array_type *elem_array =
                        (array_type *)(element_type->unqual());
                element_type = elem_array->elem_type();
              }
          }

        if (ar->elem_size() == 0)
          {
            error_line(1, ar->parent(),
                       "array reference with unknown element size cannot be "
                       "converted into C");
          }

        ptr_type *new_ptr = element_type->ptr_to();
        artree->addchild(force_type(base_tree, new_ptr));

        if (flatten)
          {
            ctree *index_tree = operand_to_tree(ar->index(0));
            unsigned num_dims = ar->dims();
            for (unsigned dim_num = 1; dim_num < num_dims; ++dim_num)
              {
                ctree *mul_tree = new ctree(ctree_mult);
                mul_tree->addchild(index_tree);
                mul_tree->addchild(operand_to_tree(ar->bound(dim_num)));

                index_tree = new ctree(ctree_add);
                index_tree->addchild(mul_tree);
                index_tree->addchild(operand_to_tree(ar->index(dim_num)));
              }

            if (!ar->offset_op().is_null())
              {
                ctree *sub_tree = new ctree(ctree_sub);
                sub_tree->addchild(index_tree);
                sub_tree->addchild(operand_to_tree(ar->offset_op()));
                index_tree = sub_tree;
              }

            artree->addchild(index_tree);
          }
        else
          {
            for (unsigned dim_num = 0; dim_num < ar->dims(); ++dim_num)
                artree->addchild(operand_to_tree(ar->index(dim_num)));
          }

        type_node *final_elem_type = base_array;
        unsigned num_dims = ar->dims();
        for (unsigned dim_num = 0; dim_num < num_dims; ++dim_num)
          {
            if (!final_elem_type->unqual()->is_array())
              {
                error_line(1, ar->parent(),
                           "array type mismatches array reference "
                           "instruction");
              }
            array_type *this_array = (array_type *)(final_elem_type->unqual());
            final_elem_type = this_array->elem_type();
          }

        immed_list *field_immeds = (immed_list *)(ar->peek_annote(k_fields));
        tree = get_address_with_offset(artree, final_elem_type,
                                       ar->result_type(), ar->offset(),
                                       field_immeds);

        return tree;
      }
    case inf_cal:
      {
        in_cal *cal = (in_cal*)inst;
        tree = new ctree(ctree_funcall);
        tree->addchild(operand_to_tree(cal->addr_op()));
        for (unsigned arg_num = 0; arg_num < cal->num_args(); ++arg_num)
            tree->addchild(operand_to_tree(cal->argument(arg_num)));
        return tree;
      }
    case inf_gen:
      {
        in_gen *the_gen = (in_gen *)inst;
        tree = new ctree(ctree_macro, the_gen->name(), the_gen->result_type());
        immed_list *arg_immeds =
                (immed_list *)(the_gen->peek_annote(k_builtin_args));
        if (arg_immeds == NULL)
          {
            for (unsigned src_num = 0; src_num < the_gen->num_srcs();
                 ++src_num)
              {
                tree->addchild(operand_to_tree(the_gen->src_op(src_num)));
              }
          }
        else
          {
            immed_list_iter arg_iter(arg_immeds);
            while (!arg_iter.is_empty())
              {
                immed this_arg = arg_iter.step();
                if (this_arg.is_type())
                  {
                    ctree *type_tree =
                            new ctree(ctree_type_ref, FALSE, this_arg.type());
                    tree->addchild(type_tree);
                  }
                else if (this_arg.is_string())
                  {
                    ctree *string_tree =
                            new ctree(ctree_strconst, this_arg.string());
                    tree->addchild(string_tree);
                  }
                else if (this_arg.is_integer())
                  {
                    unsigned arg_num = this_arg.integer();
                    if (arg_num < the_gen->num_srcs())
                      {
                        tree->addchild(operand_to_tree(the_gen->src_op(
                                arg_num)));
                      }
                    else
                      {
                        error_line(1, inst->owner(),
                                   "badly formed \"%s\" annote",
                                   k_builtin_args);
                      }
                  }
                else
                  {
                    error_line(1, inst->owner(), "badly formed \"%s\" annote",
                               k_builtin_args);
                  }
              }
          }
        return tree;
      }
    default:
        assert(FALSE);
        return NULL;
    }
}

/*
 * Generate C code for a loop (while, do...while)
 */

ctree *process_loop(tree_loop *loop)
{
    label_sym *old_continue = current_continue;
    label_sym *old_break = current_break;

    current_break = loop->brklab();

    ctree *conditional_expr = NULL;
    ctree *test_tree =
            process_node_list(loop->test(), loop->toplab(), &conditional_expr);

    if ((test_tree->getop() == ctree_semi) && (test_tree->N() == 0))
        current_continue = loop->contlab();

    ctree *body_tree = process_node_list(loop->body(), NULL, NULL);

    ctree *tree = new ctree(ctree_do);
    tree->add_object_comments(loop);

    ctree *block_body = new ctree(ctree_block);
    ctree *body_list = new ctree(ctree_semi);
    block_body->addchild(body_list);

    if (loop->toplab()->get_annote(k_s2c_label_name_used) != NULL)
        body_list->addchild(new ctree(ctree_label, loop->toplab()));
    body_list->addchild(body_tree);
    if (loop->contlab()->get_annote(k_s2c_label_name_used) != NULL)
        body_list->addchild(new ctree(ctree_label, loop->contlab()));
    body_list->addchild(test_tree);
    tree->addchild(block_body);

    if (conditional_expr == NULL)
        conditional_expr = ldc_tree(type_signed, immed(0));
    tree->addchild(conditional_expr);

    current_continue = old_continue;
    current_break = old_break;
    return tree;
}


/*
 * Generate C code for a for loop
 */

ctree *process_for(tree_for *tfor)
{
    label_sym *old_continue = current_continue;
    label_sym *old_break = current_break;

    current_break = tfor->brklab();
    current_continue = tfor->contlab();

    ctree *tree = new ctree(ctree_for);
    tree->add_object_comments(tfor);

    ctree *lbpart = new ctree(ctree_assign);
    lbpart->addchild(ctree_for_for_index(tfor->index()));
    lbpart->addchild(operand_to_tree(tfor->lb_op()));
    tree->addchild(lbpart);

    boolean is_signed = FALSE;
    if (tfor->index()->type()->is_base())
      {
        base_type *the_base_type = (base_type *)(tfor->index()->type());
        is_signed = the_base_type->is_signed();
      }

    if ((((tfor->test() == FOR_SGT) || (tfor->test() == FOR_SGTE) ||
          (tfor->test() == FOR_SLT) || (tfor->test() == FOR_SLTE)) &&
         !is_signed) ||
        (((tfor->test() == FOR_UGT) || (tfor->test() == FOR_UGTE) ||
          (tfor->test() == FOR_ULT) || (tfor->test() == FOR_ULTE)) &&
        is_signed))
      {
        error_line(1, tfor, "sign of index doesn't match TREE_FOR test");
      }

  ctree_op comparison_op = (ctree_op)0;
    switch (tfor->test())
      {
        case FOR_EQ:
            comparison_op = ctree_eq;
            break;
        case FOR_NEQ:
            comparison_op = ctree_neq;
            break;
        case FOR_SGT:
        case FOR_UGT:
            comparison_op = ctree_gt;
            break;
        case FOR_SGTE:
        case FOR_UGTE:
            comparison_op = ctree_gte;
            break;
        case FOR_SLT:
        case FOR_ULT:
            comparison_op = ctree_lt;
            break;
        case FOR_SLTE:
        case FOR_ULTE:
            comparison_op = ctree_lte;
            break;
        case FOR_SGELE:
        case FOR_UGELE:
            error_line(0, tfor, "GELE test in TREE_FOR found:");
            error_line(0, tfor, "    This is non-standard SUIF; it must be");
            error_line(0, tfor, "    run through the ``porky -defaults''");
            error_line(1, tfor, "    pass before s2c can handle it.");
        default:
            assert(FALSE);
      }

    ctree *reltest = new ctree(comparison_op);
    reltest->addchild(ctree_for_for_index(tfor->index()));
    reltest->addchild(operand_to_tree(tfor->ub_op()));
    tree->addchild(reltest);

    int thestep;
    ctree *steptree = NULL;
    if (tfor->step_is_constant(&thestep)) {
        if (thestep == 1) {
            steptree = new ctree(ctree_postinc);
            steptree->addchild(ctree_for_for_index(tfor->index()));
        }
        else if (thestep == -1) {
            steptree = new ctree(ctree_postdec);
            steptree->addchild(ctree_for_for_index(tfor->index()));
        }
        else {
            if (thestep >= 0) {
                if (thestep == 0)
                    warning_line(tfor, "``for'' loop with zero step size");
                steptree = new ctree(ctree_addassign);
                steptree->addchild(ctree_for_for_index(tfor->index()));
                steptree->addchild(ldc_tree(tfor->index()->type(),
                                            immed(thestep)));
            }
            else {
                steptree = new ctree(ctree_subassign);
                steptree->addchild(ctree_for_for_index(tfor->index()));
                steptree->addchild(ldc_tree(tfor->index()->type(),
                                            immed(-thestep)));
            }
        }
    }
    else {
        steptree = new ctree(ctree_addassign);
        steptree->addchild(ctree_for_for_index(tfor->index()));
        steptree->addchild(operand_to_tree(tfor->step_op()));
    }
    assert(steptree);
    tree->addchild(steptree);
    ctree *body_blk = new ctree(ctree_block);
    body_blk->addchild(process_node_list(tfor->body(), NULL, NULL));
    tree->addchild(body_blk);

    current_continue = old_continue;
    current_break = old_break;

    if ((tfor->landing_pad() != NULL) && (!tfor->landing_pad()->is_empty()))
      {
        ctree *bound_test = new ctree(comparison_op);
        bound_test->addchild(operand_to_tree(tfor->lb_op()));
        bound_test->addchild(operand_to_tree(tfor->ub_op()));

        ctree *body_tree = new ctree(ctree_semi);
        body_tree->addchild(process_node_list(tfor->landing_pad(), NULL,
                                              NULL));
        body_tree->addchild(tree);

        ctree *then_tree = new ctree(ctree_block);
        then_tree->addchild(body_tree);

        ctree *new_if = new ctree(ctree_if);
        new_if->addchild(bound_test);
        new_if->addchild(then_tree);

        tree = new_if;
      }

    return tree;
}


/*
 * Generate C code for an if
 */

ctree *process_if(tree_if *treeif)
{
    ctree *semi_tree = new ctree(ctree_semi);

    ctree *conditional_expr = NULL;
    semi_tree->addchild(process_node_list(treeif->header(), treeif->jumpto(),
                                          &conditional_expr));

    ctree *tree = new ctree(ctree_if);
    tree->add_object_comments(treeif);

    if (conditional_expr == NULL)
      {
        conditional_expr = ldc_tree(type_signed, immed(1));
      }
    else
      {
        ctree *not_expr = new ctree(ctree_not);
        not_expr->addchild(conditional_expr);
        conditional_expr = not_expr;
      }
    tree->addchild(conditional_expr);

    ctree *then_tree = new ctree(ctree_block);
    then_tree->addchild(process_node_list(treeif->then_part(), NULL, NULL));
    tree->addchild(then_tree);
    semi_tree->addchild(tree);
    if (!treeif->else_part()->is_empty()) {
        ctree *else_tree = new ctree(ctree_block);
        ctree *else_semi = new ctree(ctree_semi);
        if (treeif->jumpto()->get_annote(k_s2c_label_name_used) != NULL)
            else_semi->addchild(new ctree(ctree_label, treeif->jumpto()));
        else_semi->addchild(process_node_list(treeif->else_part(), NULL,
                                              NULL));
        else_tree->addchild(else_semi);
        tree->addchild(else_tree);
    } else {
        if (treeif->jumpto()->get_annote(k_s2c_label_name_used) != NULL)
            semi_tree->addchild(new ctree(ctree_label, treeif->jumpto()));
    }
    return semi_tree;
}

/*
 * Process a nested block
 */

ctree *process_block(tree_block *block)
{
    preprocess_symtab(block->symtab());

    ctree *ret = new ctree(ctree_block);
    ret->add_object_comments(block);

    ctree *retsemi = new ctree(ctree_semi);
    ret->addchild(retsemi);

    add_typedecls(retsemi, block->symtab());
    add_vardecls(retsemi, block->symtab());
    add_symtab_annotes(retsemi, block->symtab());

    add_pound_lines_for_object(block->symtab(), retsemi);

    retsemi->addchild(process_node_list(block->body(), NULL, NULL));

    return ret;
}



static void add_typedecls(ctree *ret, base_symtab *the_symtab)
{
    assert((ret != NULL) && (the_symtab != NULL));

    type_node_list_iter first_iter(the_symtab->types());
    while (!first_iter.is_empty()) {
        type_node *type = first_iter.step();
        if (type->annotes()->peek_annote(k_s2c_needs_forward_declaration) !=
            NULL) {
            ctree *typedecl = new ctree(ctree_typeforward, type);
            ret->addchild(typedecl);
        }
    }

    type_node_list_iter second_iter(the_symtab->types());
    while (!second_iter.is_empty()) {
        type_node *type = second_iter.step();
        if (type->is_named()) {
            ctree *typedecl = new ctree(ctree_typedecl, type);
            ret->addchild(typedecl);
        }
    }
}

static void add_vardecls(ctree *ret, base_symtab *the_symtab)
{
    assert((ret != NULL) && (the_symtab != NULL));

    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_symbol = sym_iter.step();
        if (this_symbol->is_var())
          {
            var_sym *the_var = (var_sym *)this_symbol;
            if (the_var->parent_var() != NULL)
                continue;

            if ((!the_var->is_param()) && the_var->is_global() &&
                (!the_var->is_private()))
              {
                ctree *the_vardecl = new ctree(ctree_vardecl, the_var);
                ret->addchild(the_vardecl);
              }
            else if ((!the_var->is_param()) && (!the_var->is_global()) &&
                     the_var->is_auto())
              {
                ctree *the_vardef = new ctree(ctree_vardef, the_var);
                ret->addchild(the_vardef);
              }
          }
        else if (this_symbol->is_proc())
          {
            proc_sym *this_proc = (proc_sym *)this_symbol;
            ctree *the_funcdecl = new ctree(ctree_funcdecl, this_proc);
            ret->addchild(the_funcdecl);
          }
      }

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        assert(this_def->variable() != NULL);
        var_sym *this_var = this_def->variable();
        if (this_var->annotes()->peek_annote(k_s2c_one_use) == NULL)
          {
            ctree *the_vardef = new ctree(ctree_vardef, this_var);
            the_vardef->add_object_comments(this_def);
            ret->addchild(the_vardef);
          }
      }
}

static void add_symtab_annotes(ctree *ret, base_symtab *the_symtab)
{
    if (the_symtab->peek_annote(k_s2c_comments) == NULL)
        return;

    char *comment = (the_symtab->is_global() ?
                     (the_symtab->is_file() ? (char*)" file symtab annotes: " :
                                              (char*)" global symtab annotes: ") :
                     (char*)" symtab annotes: ");

    ctree *symtab_annote_tree = new ctree(ctree_blank_line);
    symtab_annote_tree->add_comment(comment);
    symtab_annote_tree->add_object_comments(the_symtab);
    ret->addchild(symtab_annote_tree);
    ret->addchild(new ctree(ctree_blank_line));
}

static boolean operand_is_intconst(operand the_operand, int *the_const)
{
    if (!the_operand.is_expr())
        return FALSE;
    instruction *the_instr = the_operand.instr();

    if (the_instr->opcode() != io_ldc)
        return FALSE;
    in_ldc *the_ldc = (in_ldc *)the_instr;

    immed value = the_ldc->value();
    if (!value.is_integer())
        return FALSE;

    *the_const = value.integer();
    return TRUE;
}

static ctree *create_goto(label_sym *the_label)
{
    if (the_label == current_break)
      {
        return new ctree(ctree_break);
      }
    else if (the_label == current_continue)
      {
        return new ctree(ctree_continue);
      }
    else
      {
        the_label->append_annote(k_s2c_label_name_used, the_label);
        return new ctree(ctree_goto, the_label);
      }
}

static ctree *create_branch(label_sym *the_label, ctree *condition)
{
    ctree *the_goto = create_goto(the_label);

    i_integer value;
    boolean is_const = condition->is_int_const(&value);
    if (is_const && (value == 1))
      {
        the_goto->copy_comments_from(condition);
        delete condition;
        return the_goto;
      }

    ctree *result = new ctree(ctree_if);
    result->addchild(condition);
    result->addchild(the_goto);

    return result;
}

/*
 *  If a sub-variable occurs as the index of a tree_for, we can't
 *  replace it in the SUIF code, because we need a variable as an
 *  index.  So here we deal with it as we're converting to C code.
 */
static ctree *ctree_for_for_index(var_sym *index)
{
    if (index->parent_var() == NULL)
        return new ctree(ctree_symconst, index);

    in_rrr *new_copy =
            new in_rrr(io_cpy, index->type()->unqual(), operand(),
                       operand(index));
    instr_no_sub_vars(new_copy);
    ctree *result = operand_to_tree(operand(new_copy));
    delete new_copy;
    return result;
}

static void print_pound_lines_for_preamble(suif_object *the_object)
{
    instruction *pound_hanger = NULL;
    annote_list_iter annote_iter(the_object->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        if (strcmp(this_annote->name(), k_s2c_preamble_pragma) == 0)
          {
            if (pound_hanger == NULL)
                pound_hanger = new in_rrr(io_mrk);
            immed_list *new_data = new immed_list;
            immed_list_iter data_iter(this_annote->immeds());
            while (!data_iter.is_empty())
              {
                immed this_immed = data_iter.step();
                new_data->append(this_immed);
              }
            pound_hanger->append_annote(k_s2c_pragma, new_data);
          }
      }

    if (pound_hanger != NULL)
      {
        ctree *dummy_ctree = new ctree(ctree_semi);
        add_pound_lines_for_object(pound_hanger, dummy_ctree);
        file_io the_io(c_file);
        transform_and_print_ctree(&the_io, dummy_ctree);
        delete dummy_ctree;
        delete pound_hanger;
      }
}

static void add_pound_lines_for_object(suif_object *the_object,
                                       ctree *parent_ctree)
{
    annote_list_iter first_iter(the_object->annotes());
    while (!first_iter.is_empty())
      {
        annote *this_annote = first_iter.step();
        if (strcmp(this_annote->name(), k_s2c_pragma) == 0)
          {
            this_annote->immeds()->push("pragma");
            this_annote->set_name(k_s2c_pound_line);
          }
      }

    annote_list_iter annote_iter(the_object->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        if (strcmp(this_annote->name(), k_s2c_pound_line) == 0)
          {
            char *pound_string;
            string_io *the_io = new string_io(&pound_string);
            immed_list *data = this_annote->immeds();
            immed_list_iter data_iter(data);
            boolean first = TRUE;
            while (!data_iter.is_empty())
              {
                if (first)
                    first = FALSE;
                else
                    the_io->printf(" ");

                immed this_immed = data_iter.step();
                switch (this_immed.kind())
                  {
                    case im_int:
                        the_io->printf("%d", this_immed.integer());
                        break;
                    case im_extended_int:
                        the_io->printf("%s", this_immed.ext_integer());
                        break;
                    case im_string:
                        the_io->printf("%s", this_immed.string());
                        break;
                    case im_float:
                        the_io->printf("%g", this_immed.flt());
                        break;
                    case im_extended_float:
                        the_io->printf("%s", this_immed.ext_flt());
                        break;
                    case im_symbol:
                        if (this_immed.offset() != 0)
                            the_io->printf("<");
                        the_io->printf("%s", this_immed.symbol()->name());
                        if (this_immed.offset() != 0)
                            the_io->printf(",%d>", this_immed.offset());
                        break;
                    case im_type:
                        the_io->printf("%s", make_c_type(this_immed.type()));
                        break;
                    case im_op:
                    case im_instr:
                      {
                        boolean old_in_pound_line = in_pound_line;
                        in_pound_line = TRUE;

                        ctree *the_ctree;
                        if (this_immed.is_op())
                          {
                            comment_operand(this_immed.op());
                            the_ctree = operand_to_tree(this_immed.op());
                          }
                        else
                          {
                            comment_instr(this_immed.instr());
                            the_ctree = process_solo_instr(this_immed.instr());
                          }
                        transform_and_print_ctree(the_io, the_ctree);
                        delete the_ctree;

                        in_pound_line = old_in_pound_line;

                        break;
                      }
                    case im_undef:
                        the_io->printf("?");
                        break;
                    default:
                        assert(FALSE);
                  }
              }
            delete the_io;
            const char *installed_string = lexicon->enter(pound_string)->sp;
            delete[] pound_string;
            parent_ctree->addchild(new ctree(ctree_pound_line,
                                             installed_string));
          }
      }
}

static void fix_macro_name(macro_op_data *the_macro)
{
    const char *old_name = the_macro->op_name;
    if (!is_interfile_name(old_name))
        return;
    char *new_name = new char[strlen(old_name) + 20];
    strcpy(new_name, old_name);
    char *num_place = &(new_name[strlen(old_name)]);
    int number = 1;
    do
      {
        sprintf(num_place, "%d", number);
        ++number;
      } while (is_interfile_name(new_name));
    the_macro->op_name = new_name;
}

static boolean is_interfile_name(const char *the_name)
{
    if (fileset->globals()->lookup_sym(the_name, SYM_PROC) != NULL)
        return TRUE;
    if (fileset->globals()->lookup_sym(the_name, SYM_LABEL) != NULL)
        return TRUE;
    if (fileset->globals()->lookup_sym(the_name, SYM_VAR) != NULL)
        return TRUE;
    return FALSE;
}

static void fix_relational_op(in_rrr *the_rrr)
{
    /* In C, relational operators have additional type
     * restrictions beyond what SUIF requires.  In particular,
     * pointer comparisons are restricted. */
    switch (the_rrr->opcode())
      {
        case io_seq:
        case io_sne:
        case io_sl:
        case io_sle:
            break;
        default:
            return;
      }
    if (!the_rrr->src1_op().type()->is_ptr())
        return;
    assert(the_rrr->src2_op().type()->is_ptr());
    ptr_type *p1 = (ptr_type *)(the_rrr->src1_op().type());
    ptr_type *p2 = (ptr_type *)(the_rrr->src2_op().type());
    type_node *ref1 = p1->ref_type();
    type_node *ref2 = p2->ref_type();
    if (ref1->is_func() || ref2->is_func())
      {
        boolean use_int_comparison = FALSE;
        if ((the_rrr->opcode() == io_sl) || (the_rrr->opcode() == io_sle))
          {
            /* Cast both to integers. */
            use_int_comparison = TRUE;
          }
        else if ((!ref1->is_func()) || (!ref2->is_func()))
          {
            if ((!is_null_pointer_const(the_rrr->src1_op())) &&
                (!is_null_pointer_const(the_rrr->src2_op())))
              {
                /* Cast both to integers. */
                use_int_comparison = TRUE;
              }
          }
        else
          {
            if (composite(ref1, ref2) == NULL)
              {
                /* Cast op2 to op1's type */
                operand src2_op = the_rrr->src2_op();
                src2_op.remove();
                src2_op = operand(new in_rrr(io_cvt, p1, operand(), src2_op));
                the_rrr->set_src2(src2_op);
              }
          }
        if (use_int_comparison)
          {
            /* Cast both to integers. */

            operand src1_op = the_rrr->src1_op();
            src1_op.remove();
            src1_op = operand(new in_rrr(io_cvt, type_unsigned_longlong,
                                         operand(), src1_op));
            the_rrr->set_src1(src1_op);

            operand src2_op = the_rrr->src2_op();
            src2_op.remove();
            src2_op = operand(new in_rrr(io_cvt, type_unsigned_longlong,
                                         operand(), src2_op));
            the_rrr->set_src2(src2_op);
          }
      }
    else
      {
        if ((the_rrr->opcode() == io_sl) || (the_rrr->opcode() == io_sle))
          {
            if ((composite(ref1, ref2) != NULL) &&
                ((ref1->size() != 0) == (ref2->size() != 0)))
              {
                /* Do nothing, the comparison is ok. */
              }
            else
              {
                /* Cast both to ``char *''. */

                operand src1_op = the_rrr->src1_op();
                src1_op.remove();
                src1_op = operand(new in_rrr(io_cvt, type_char->ptr_to(),
                                             operand(), src1_op));
                the_rrr->set_src1(src1_op);

                operand src2_op = the_rrr->src2_op();
                src2_op.remove();
                src2_op = operand(new in_rrr(io_cvt, type_char->ptr_to(),
                                             operand(), src2_op));
                the_rrr->set_src2(src2_op);
              }
          }
        else
          {
            if (composite(ref1, ref2) != NULL)
              {
                /* Do nothing, the comparison is ok. */
              }
            else if ((ref1->unqual()->op() == TYPE_VOID) ||
                     (ref2->unqual()->op() == TYPE_VOID))
              {
                /* Do nothing, the comparison is ok. */
              }
            else if (is_null_pointer_const(the_rrr->src1_op()))
              {
                /* Do nothing, the comparison is ok. */
              }
            else if (is_null_pointer_const(the_rrr->src2_op()))
              {
                /* Do nothing, the comparison is ok. */
              }
            else
              {
                /* Cast one to ``void *''. */
                operand src2_op = the_rrr->src2_op();
                src2_op.remove();
                src2_op = operand(new in_rrr(io_cvt, type_void->ptr_to(),
                                             operand(), src2_op));
                the_rrr->set_src2(src2_op);
              }
          }
      }
}

static boolean is_null_pointer_const(operand the_op)
{
    if (!the_op.is_expr())
        return FALSE;
    instruction *the_instr = the_op.instr();
    while (the_instr->opcode() == io_cvt)
      {
        in_rrr *the_convert = (in_rrr *)the_instr;
        type_node *result_type = the_convert->result_type();
        operand new_op = the_convert->src_op();
        if (result_type->op() == TYPE_INT)
          {
            if (new_op.type()->is_ptr())
                return FALSE;
          }
        else if (result_type->is_ptr())
          {
            ptr_type *result_ptr = (ptr_type *)result_type;
            if (result_ptr->ref_type()->op() != TYPE_VOID)
                return FALSE;
          }
        else
          {
            return FALSE;
          }
        if (!new_op.is_expr())
            return FALSE;
        the_instr = new_op.instr();
      }
    if (the_instr->opcode() != io_ldc)
        return FALSE;
    in_ldc *the_ldc = (in_ldc *)the_instr;
    type_node *result_type = the_ldc->result_type();
    if (result_type->op() == TYPE_INT)
      {
        /* OK, no problem in this case. */
      }
    else if (result_type->is_ptr())
      {
        ptr_type *result_ptr = (ptr_type *)result_type;
        if (result_ptr->ref_type()->op() != TYPE_VOID)
            return FALSE;
      }
    else
      {
        return FALSE;
      }
    return (the_ldc->value() == immed(0));
}
