/*
 * NOTE: This example does not work under cygwin.
 */

#include "TaskGraph"
#include "TaskUserFunctions.h"
#include <cstdio>
#include <vector>
#include <map>

#define MAX_EXPRESSION_ARGS 3
#define Register_Command(name, arity) \
commandVector.push_back( mathCommand ( (name), (arity) ) ); \
	commandDictionary[(name)] = commandVector.size()-1;

using namespace tg;


struct mathExpr;
struct mathCommand;
struct ltstr {
	bool operator()(const char* s1, const char* s2) const
	{
		return strcmp(s1, s2) < 0;
	}
};


static std::vector<mathExpr> expressionVector;
static std::vector<mathCommand> commandVector;
static std::map< const char*, unsigned, ltstr > commandDictionary;

static int global_id = 0;

extern "C" int tg_add ( int a, int b ) {
	return a+b;
}

extern "C" int tg_ifz ( int b, int x, int y ) {
	return b?x:y;
}

extern "C" int tg_mgc ( int x ) {
	return x?x+tg_mgc(x-1):0;
}

extern "C" int tg_div ( int x, int y ) {
	return x/y;
}

extern "C" int tg_mul ( int x, int y ) {
	return x*y;
}


struct mathCommand {
	friend struct mathExpr;
	public:
	mathCommand ( ) : id(0), arity(0), name(NULL) { }
	mathCommand ( const char *_name, int _arity ) {
		id = ++global_id;
		arity = _arity;
		name = new char[strlen(_name)+1];
		strcpy ( name, _name );
		switch ( arity ) {
			case 1:
				address = new TaskFunction1<int, int> ( name );
				break;
			case 2:
				address = new TaskFunction2<int, int, int> ( name );
				break;
			case 3:
				address = new TaskFunction3<int, int, int, int> ( name );
				break;
			default:
				printf ( "Error: No support for functions with more than 4 arguments!\n" );
				address = NULL;
		}
	}
	mathCommand ( const mathCommand& obj ) {
		id = obj.id;
		arity = obj.arity;
		name = new char[strlen(obj.name)+1];
		strcpy ( name, obj.name );
		address = obj.address;
	}
	const mathCommand& operator= ( const mathCommand& obj ) {
		if (this != &obj) {
			id = obj.id;
			arity = obj.arity;
			name = new char[strlen(obj.name)+1];
			strcpy ( name, obj.name );
			address = obj.address;
		}
		return *this;
	}
	unsigned id;
	unsigned arity;
	char *name;
	void *address;
};

struct mathExpr {
	friend struct mathCommand;
	public:
	mathExpr ( ) : evaluated ( false ), arity ( 0 ) { };
	mathExpr ( int _value ) : id ( 0 ), value ( TaskExpression(_value) ), evaluated ( true ), arity ( 0 ) { }
	mathExpr ( TaskExpression &_expr ) : id ( 0 ), value ( _expr ), evaluated ( true ), arity( 0 ) { }
	mathExpr ( TaskScalarVariable &_expr ) : id ( 0 ), value ( TaskExpression(_expr) ), evaluated ( true ), arity( 0 ) { }
	mathExpr ( const char *name, mathExpr _arg1 ) : id ( commandDictionary[name] ), value ( 0 ), evaluated ( false ), arity ( 1 ) {
		expressionVector.push_back ( _arg1 );
		arguments[0] = expressionVector.size()-1;
	};
	mathExpr ( const char *name, mathExpr _arg1, mathExpr _arg2 ) : id ( commandDictionary[name] ), value ( 0 ), evaluated ( false ), arity ( 2 ) {
		expressionVector.push_back ( _arg1 );
		arguments[0] = expressionVector.size()-1;
		expressionVector.push_back ( _arg2 );
		arguments[1] = expressionVector.size()-1;
	};
	mathExpr ( const char *name, mathExpr _arg1, mathExpr _arg2, mathExpr _arg3 ) : id ( commandDictionary[name] ), value ( 0 ), evaluated ( false ), arity ( 3 ) {
		expressionVector.push_back ( _arg1 );
		arguments[0] = expressionVector.size()-1;
		expressionVector.push_back ( _arg2 );
		arguments[1] = expressionVector.size()-1;
		expressionVector.push_back ( _arg3 );
		arguments[2] = expressionVector.size()-1;
	};
	mathExpr ( const mathExpr& obj ) {
		id = obj.id;
		value = obj.value;
		evaluated = obj.evaluated;
		arity = obj.arity;
		for ( int i = 0; i < MAX_EXPRESSION_ARGS; i++ ) arguments[i] = obj.arguments[i];
	}
	const mathExpr& operator= ( const mathExpr& obj ) {
		if (this != &obj) {
			id = obj.id;
			value = obj.value;
			evaluated = obj.evaluated;
			arity = obj.arity;
			for ( int i = 0; i < MAX_EXPRESSION_ARGS; i++ ) arguments[i] = obj.arguments[i];
		}
		return *this;
	}
	TaskExpression eval ( ) {
		if ( evaluated ) {
			return value;
		} else {
			switch ( arity ) {
				case 1:
					return (reinterpret_cast<TaskFunction1<int,int>*>(commandVector[id].address))->call ( expressionVector[arguments[0]].eval() );
				case 2:
					return (reinterpret_cast<TaskFunction2<int,int,int>*>(commandVector[id].address))->call ( expressionVector[arguments[0]].eval(), expressionVector[arguments[1]].eval() );
				case 3:
					return (reinterpret_cast<TaskFunction3<int,int,int,int>*>(commandVector[id].address))->call ( expressionVector[arguments[0]].eval(), expressionVector[arguments[1]].eval(), expressionVector[arguments[2]].eval() );
				default:
					printf ( "Error: No support for functions with more than 4 arguments!\n" );
					return TaskExpression ( );
			}
		}
	}
	unsigned id;
	TaskExpression value;
	bool evaluated;
	unsigned arity;
	unsigned arguments[MAX_EXPRESSION_ARGS];
};

void script_error ( const char* script, int index ) {
	printf ( "Parse Error: %s\n", script);
	printf ( "Position:    ");
	for ( int i = 0; i < index; i++ ) putchar(' ');
	printf ( "^ here\n");
}

void ignore_space ( const char *script, unsigned &i ) {
	while ( isspace(script[i]) ) i++;
}

bool script_next ( const char *script, unsigned &i, bool closing) {
	if ( closing ) {
		if ( script[i] != ')' ) {
			printf ( "Error, expected \")\".\n" );
			script_error(script, i);
			return false;
		} else {
			i++;
		}
	} else {
		if ( script[i] != ',' ) {
			printf ( "Error, expected \",\".\n" );
			script_error(script, i);
			return false;
		} else {
			i++;
		}
	}
	ignore_space ( script, i );
	return true;
}

mathExpr parser ( const char* script, unsigned &i, unsigned nvars, TaskScalarVariable **vars, bool closing = false , bool first = false ) {
	int intvalue = 0, j = 0, k = 0;
	int sign = 1;
	char name[31];
	bool isvalue = false;
	unsigned arity = 0, n;
	std::map<const char*, unsigned>::iterator comm;
	mathExpr mExpr;

	if ( isdigit( script[i] ) || script[i] == '-' || script[i] == '+' ) {
		isvalue = true;
		if ( script[i] == '-' ) {
			sign = -1;
			i++;
			if ( !isdigit(script[i]) ) {
				printf("Error, expected digit after sign.\n");
				script_error(script, i);
				return mathExpr();
			}
		} else if ( script[i] == '+' ) {
			i++;
			if ( !isdigit(script[i]) ) {
				printf("Error, expected digit after sign.\n");
				script_error(script, i);
				return mathExpr();
			}
		}
		while ( script[i] && isdigit(script[i]) ) intvalue = intvalue*10 + script[i++]-'0';
		intvalue *= sign;
	} else if ( isalpha(script[i]) || script[i] == '_' || isdigit(script[i]) ) {
		j = 0;
		while ( ( isalpha(script[i]) || script[i] == '_' || isdigit(script[i]) ) && j < 31 ) name[j++] = script[i++];
		name[j] = '\0';
	} else {
		script_error(script, i);
		return mathExpr();
	}
	ignore_space ( script, i );

	if ( isvalue ) {
		if ( !script_next(script, i, closing) ) {
			return mathExpr();
		}
		return mathExpr ( intvalue );
	} else {

		if ( j > 3 && name[0] == 'a' && name[1] == 'r' && name[2] == 'g' && isdigit(name[3]) ) {
			k = 3;
			n = 0;
			while ( isdigit(name[k]) ) n = n*10 + name[k++]-'0';
			if ( n > nvars ) {
				printf ( "Error, arg%d is not provided.", n );
				script_error(script, i);
				return mathExpr ();
			}
			if ( !script_next(script, i, closing) ) {
				return mathExpr();
			}
			return mathExpr ( *(vars[n]) );
		} else {
			if ( script[i] != '(' ) {
				printf ( "Error: too long command name or command %s not followed by \"(\"!\n", name );
				script_error(script, i);
				return mathExpr();
			}
			i++;
			ignore_space ( script, i );
			comm = commandDictionary.find ( name );
			if ( comm != commandDictionary.end() ) {
				arity = commandVector[commandDictionary[name]].arity;
			} else {
				printf ( "Error: command \"%s\" not found!\n", name );
				script_error(script, i);
				return mathExpr();
			}
			switch ( arity ) {
				case 1:
					{
						mathExpr mExpr1( name, parser(script, i, nvars, vars, true) );
						if ( !first && !script_next(script, i, closing) ) {
							return mathExpr();
						}
						return mExpr1;
					}
				case 2:
					{
						mathExpr lhs = parser(script, i, nvars, vars);
						mathExpr rhs = parser(script, i, nvars, vars, true);
						mathExpr mExpr2( name, lhs, rhs );
						if ( !first && !script_next(script, i, closing) ) {
							return mathExpr();
						}
						return mExpr2;
					}
				case 3:
					{
						mathExpr lhs = parser(script, i, nvars, vars);
						mathExpr middle = parser(script, i, nvars, vars);
						mathExpr rhs = parser(script, i, nvars, vars, true);
						mathExpr mExpr3( name, lhs, middle, rhs );
						if ( !first && !script_next(script, i, closing) ) {
							return mathExpr();
						}
						return mExpr3;
					}
				default:
					printf ( "Error: No support for functions with more than 4 arguments!\n" );
					return mathExpr ( );
			}
		}

	}

	return mathExpr ( );
}

typedef TaskGraph<int, int, int, int, int, int> TG;

void interpret ( TG &t, const char *filename ) {
	FILE *fp;
	char script[1000];
	unsigned index = 0;

	fp = fopen ( filename, "r" );
	fgets ( script, 1000, fp );
	fclose ( fp );
	taskgraph( TG, t, tuple5( input1, input2, input3, input4, input5 ) ) {
		TaskScalarVariable* vars[5];
		vars[0] = &input1;
		vars[1] = &input2;
		vars[2] = &input3;
		vars[3] = &input4;
		vars[4] = &input5;
		tReturn( parser(script, index, 5, vars, false, true ).eval() );
	}
}

int main ( int argc, char **argv ) {
#ifdef ENV_CYGWIN
	fprintf(stderr, "This example does not work in cygwin due to linking issues in Windows\n");
	return -1;
#endif
	Register_Command("tg_add", 2);
	Register_Command("tg_ifz", 3);
	Register_Command("tg_mgc", 1);
	Register_Command("tg_div", 2);
	Register_Command("tg_mul", 2);

	TG t;
	interpret ( t, "math_script" );
	t.compile ( tg::GCC, true );
	int arg1= 24;
	int arg2= 0;
	int arg3= 0;
	int arg4= 0;
	int arg5= 0;
	printf ( "result = %d\n", t.execute ( arg1, arg2, arg3, arg4, arg5 ) );

}

