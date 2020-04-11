/* Implementation of Hi-level Types */

#define _MODULE_ "libbuilder.a"
#include <cstdlib>
#include "suif.h"
#include "builder.h"


/*
 * Parse a character string into the appropriate type - the inverse of
 * printable() above.
 */

class type_template {
    type_ops oper;
    int flags;	
    int sz;
    type_template * opernd;
    type_node * Type;


    void set_const()			{ flags |= TYPE_ISCONST; }
    void set_volatile()			{ flags |= TYPE_ISVOLATILE; }
    void set_signed()			{ flags |= TYPE_ISSIGNED; }

    void reset_const()			{ flags &= ~TYPE_ISCONST; }
    void reset_volatile()		{ flags &= ~TYPE_ISVOLATILE; }   
    void reset_signed()			{ flags &= ~TYPE_ISSIGNED; }

    type_ops op() { return oper; }

    void SetType(type_node * tp) { Type = tp; }

public:
    type_template();

    static type_template * parse_type(char *str, base_symtab *symtab);
    static type_node * cvt_suif_type(if_type t, base_symtab *symtab);
    static type_template * parse_aggregate(annote *an, base_symtab *symtab);
    static type_template * parse_aggregate(char *str, base_symtab *symtab);
};

type_template::type_template()
{
    oper = TYPE_VOID;
    flags = 0;
    Type = NULL;
    opernd = NULL;
}

type_template * type_template::parse_type(char *str, base_symtab *symtab)
{
    type_ops op;
    int size;
    type_template *cur;
    char *ptr = (char*)str;
    char bfr[256];
    
    type_template *cur_type = NULL;
    boolean cur_alloced;
    type_template *old_type = NULL;
    type_template *exist_type = NULL;
    
    cur_type = new base_type;
    cur_alloced = TRUE;

    while (*ptr) {
	if (!strncmp(ptr, "const ", 6)) {
	    cur_type->set_const();
	    ptr += 6;
	    continue;
	} else if (!strncmp(ptr, "volatile ", 9)) {
	    cur_type->set_volatile();
	    ptr += 9;
	    continue;
	} else if (!strncmp(ptr, "signed ", 7)) {
	    cur_type->set_signed();
	    ptr += 7;
	    continue;
	} else if (!strncmp(ptr, "int", 3)) {
	    cur_type->oper = TYPE_INT;
	    cur_type->sz = atoi(ptr+3);
	    ptr += 3;
	    if (*ptr < '0' || *ptr > '9') {
		/* unrecognizable type string: give up and return NULL */
		if (cur_alloced) delete cur_type;
		return NULL;
	    }
	    while (*ptr >= '0' && *ptr <= '9')
		ptr++;
	} else if (!strncmp(ptr, "float", 5)) {
	    cur_type->oper = TYPE_FLOAT;
	    cur_type->sz = atoi(ptr+5);
	    ptr += 5;
	    if (*ptr < '0' || *ptr > '9') {
		/* unrecognizable type string: give up and return NULL */
		if (cur_alloced) delete cur_type;
		return NULL;
	    }
	    while (*ptr >= '0' && *ptr <= '9')
		ptr++;
	} else if (!strncmp(ptr, "void", 4)) {
	    cur_type->oper = TYPE_VOID;
	    ptr += 4;
	} else if (!strncmp(ptr, "()", 2)) {
	    cur_type->oper = TYPE_FUNC;
	    ptr += 2;
	} else if (*ptr == '*') {
	    cur_type->oper = TYPE_PTR;
	    cur_type->sz = suif_ptr_size;
	    cur_type->opernd = old_type;
	    ptr++;
	} else if (*ptr == '[') {
	    cur_type->oper = TYPE_ARRAY;
	    cur_type->opernd = old_type;
	    if (ptr[1] == ']') {
		cur_type->sz = 0;
		ptr += 2;
	    } else {
		cur_type->sz = atoi(ptr+1)*old_type->sz;
		ptr = index(ptr, ']')+1;
	    }
	} else if (!strncmp(ptr, "struct", 6)) {
	    ptr += 7;
	    strcpy(bfr, ptr);
	    char *dot = index(bfr, '.');
	    char *end = index(dot, '/');
	    ptr = end + 1;
	    *end = '\0';
	    *dot = '\0';
	    base_symtab *tsymtab = symtab->lookup_child(bfr);
	    delete cur_type;
	    cur_type->SetType(tsymtab->lookup_child(dot+1));
	    cur_alloced = FALSE;
	    assert_msg(cur_type && (cur_type->op() == TYPE_STRUCT),
		       ("type_node::parse_type - Reference to undefined structure `%s'",
			dot+1));
	} else if (!strncmp(ptr, "union", 5)) {
	    ptr += 6;
	    strcpy(bfr, ptr);
	    char *dot = index(bfr, '.');
	    char *end = index(dot, '/');
	    ptr = end + 1;
	    *end = '\0';
	    *dot = '\0';
	    base_symtab *tsymtab = symtab->lookup_child(bfr);
	    delete cur_type;
	    cur_type->SetType(tsymtab->lookup_child(dot+1));
	    cur_alloced = FALSE;
	    assert_msg(cur_type && (cur_type->op() == TYPE_UNION),
		       ("type_node::parse_type - Reference to undefined union `%s'",
			dot+1));
	} else if (!strncmp(ptr, "enum", 4)) {
	    ptr += 5;
	    strcpy(bfr, ptr);
	    char *dot = index(bfr, '.');
	    char *end = index(dot, '/');
	    ptr = end + 1;
	    *end = '\0';
	    *dot = '\0';
	    base_symtab *tsymtab = symtab->lookup_child(bfr);
	    delete cur_type;
	    cur_type->SetType(tsymtab->lookup_child(dot+1));
	    cur_alloced = FALSE;
	    assert_msg(cur_type && (cur_type->op() == TYPE_ENUM),
		       ("type_node::parse_type - Reference to undefined enum `%s'",
			dot+1));
	} else {
	    /* unrecognizable type string: give up and return NULL */
	    if (cur_alloced) delete cur_type;
	    return NULL;
	}

	if (symtab && cur_alloced) {
// BUGBUG	    exist_type = symtab->lookup_type(cur_type, TRUE);
	    if (exist_type) {
		delete cur_type;
		cur_type = exist_type;
	    } else {
//BUGBUG:		symtab->add_type(cur_type);
            }
	}
	
	old_type = cur_type;
	cur_type = new type_template;
	cur_alloced = TRUE;
    }

    if (cur_alloced)
	delete cur_type;
    
    return old_type;
}


/*
 * Convert a SUIF type into a type_node.
 */

type_template * type_template::cvt_suif_type(if_type t, base_symtab *symtab)
{
    char bfr[256];
    
    switch (t.base()) {
    case itc_error:
	assert_msg(FALSE, ("type_node::cvt_suif_type - Can't convert error type"));
	return NULL;
    case itc_record:
	assert_msg(FALSE, ("type_node::cvt_suif_type - Can't convert record type"));
	return NULL;
    case itc_void:
	return parse_type("void", symtab);
    case itc_address:
	return parse_type("void*", symtab);
    case itc_signed:
	sprintf(bfr, "signed int%d", t.size());
	return parse_type(bfr, symtab);
    case itc_unsigned:
	sprintf(bfr, "int%d", t.size());
	return parse_type(bfr, symtab);
    case itc_float:
	sprintf(bfr, "float%d", t.size());
	return parse_type(bfr, symtab);
    }
}


/*
 * Parse a character string into a struct or union type.
 */

type_template * type_template::parse_aggregate(annote *an, base_symtab *symtab)
{
    type_template *type = new type_template;
    
    if (an->name() == k_struct)
	type->oper = TYPE_STRUCT;
    else if (an->name() == k_union)
	type->oper = TYPE_UNION;
    else if (an->name() == k_enum)
	type->oper = TYPE_ENUM;
    else
	assert_msg(FALSE, ("type_node::parse_aggregate - Annote name `%s' not struct, union, or enum",
			   an->name()));

    type->my_name = (*an->immeds())[0].string();
    type->sz = (*an->immeds())[1].integer();

    /* In case struct elems refer to this struct */
    symtab->add_type(type);
    
    if (type->oper == TYPE_ENUM) {
	type->ntypes = (an->immeds()->count()-2)/2;
	type->types = NULL;
    } else {
	type->ntypes = (an->immeds()->count()-2)/3;
	type->types = new type_template*[type->ntypes];
    }

    type->names = new char*[type->ntypes];
    type->offsets = new int[type->ntypes];

    int pos = 2;
    for (int i=0; i<type->ntypes; i++) {
	type->names[i] = (*an->immeds())[pos++].string();
	type->offsets[i] = (*an->immeds())[pos++].integer();
	if (type->oper != TYPE_ENUM)
	    type->types[i] = parse_type((*an->immeds())[pos++].string(),symtab);
    }

    return type;
}


type_template * type_template::parse_aggregate(char *str, base_symtab *symtab)
{
    type_template *type = new type_template;
    
    if (!strncmp(str, "struct", 6)) {
	type->oper = TYPE_STRUCT;
	str += 7;
    } else if (!strncmp(str, "union", 5)) {
	type->oper = TYPE_UNION;
	str += 6;
    } else if (!strncmp(str, "enum", 4)) {
	type->oper = TYPE_ENUM;
	str += 5;
    } else
	assert_msg(FALSE, ("type_node::parse_aggregate - name not struct, union, or enum"));

    char *end = index(str, ' ');
    *end = '\0';
    type->my_name = lexicon->enter(str)->sp;
    *end = ' ';
    str = end + 1;

    /* grab the aggregate type size */
    end = index(str, ' ');
    type->sz = atoi(str);
    str = end + 1;

    /* grab the number of fields */
    end = index(str, ' ');
    type->ntypes = atoi(str);
    str = end + 1;

    /* In case struct elems refer to this struct */
    symtab->add_type(type);
    
    if (type->oper == TYPE_ENUM) {
	type->types = NULL;
    } else {
	type->types = new type_template*[type->ntypes];
    }

    type->names = new char*[type->ntypes];
    type->offsets = new int[type->ntypes];

    for (int i=0; i<type->ntypes; i++) {
	if (type->oper != TYPE_ENUM) {

	    assert(*str++ == '{');

	    /* grab the field name */
	    end = index(str, '@');
	    *end = '\0';
	    type->names[i] = lexicon->enter(str)->sp;
	    *end = '@';
	    str = end + 1;

	    /* grab the offset */
	    type->offsets[i] = atoi(str);
	    str = index(str, ' ') + 1;

	    /* parse the field type */
	    end = index(str, '}');
	    *end = '\0';
	    type->types[i] = parse_type(str, symtab);
	    *end = '}';
	    str = end + 2;

	} else {

	    /* grab the field name */
	    end = index(str, '=');
	    *end = '\0';
	    type->names[i] = lexicon->enter(str)->sp;
	    *end = '=';
	    str = end + 1;

	    /* grab the offset (actually the enum value) */
	    type->offsets[i] = atoi(str);
	    str = index(str, ' ') + 1;
	}
    }

    return type;
}

