/*
 * Interpreter for simple functional language
 *
 * Interpreter walks AST using visitor.  Specialiser walks ASTs generating
 * code.  Very loosely modelled on Walid Taha's "Gentle Introduction"
 * example.
 *
 * The tricky bit is getting the generated code to call other taskgraphs.
 *
 * This uses the TaskUserFunctions feature,which requires a special
 * ld option: -Wl,-export-dynamic
 *
 * So this link line works for me (or see the makefile):
 * g++ expressions.o -L/homes/phjk/TASKGRAPH/lib -L/homes/phjk/ROSE-install/lib -Wl,-export-dynamic,-rpath,/homes/phjk/ROSE-install/lib -Wl,-rpath,/vol/linux/apps/gcc/latest/lib -lTaskGraph -ldl -lrose -ledg -o expressions
 *
 * Paul Kelly  Imperial College London  October 2004
 *
 * Status: works, yields speedup of ca.100 times over the interpreter
 * for fib(29).
 *
 * NOTE: This example does not work under cygwin.
 *
 * TODO:
 *  * replace tExecuteIntToInt with direct call to T->functionaddress ?
 *  * add static map from variable name to tVar, to support multiple parameters, let etc
 *  * explore exceptions as discussed in the Gentle Introduction paper
 *
 */

#include <TaskGraph>
#include "TaskUserFunctions.h"

#include <string>
#include <iostream>
#include <map>
#include <iterator>

using namespace std;
using namespace tg;

/*
 * Expressions
 *
 * AST/visitor structure courtesy Akim Demaille:
 * http://www.lrde.epita.fr/cgi-bin/twiki/view/Projects/TigerCompiler
 */

// Fwd.
class Tree;
class Exp;
class Bin;
class Num;
class Var;
class App;
class Ifz;
class Decl;

typedef long pointer_size_t;

class voidVisitor
{
public:
  virtual ~voidVisitor() { }
  virtual void visitBin (const Bin &exp) = 0;
  virtual void visitNum (const Num &exp) = 0;
  virtual void visitVar (const Var &exp) = 0;
  virtual void visitApp (const App &exp) = 0;
  virtual void visitIfz (const Ifz &exp) = 0;
  virtual void visitDecl (const Decl &decl) = 0;
};

class intVisitor
{
public:
  virtual ~intVisitor() { }
  virtual int visitBin (const Bin &exp) = 0;
  virtual int visitNum (const Num &exp) = 0;
  virtual int visitVar (const Var &exp) = 0;
  virtual int visitApp (const App &exp) = 0;
  virtual int visitIfz (const Ifz &exp) = 0;
  virtual int visitDecl (const Decl &decl) = 0; // (makes no sense)
};

class Tree
{
public:
  virtual ~Tree()  { }
  virtual void accept (voidVisitor &v) const = 0;
  virtual int accept (intVisitor &v) const = 0;
};


class Exp : public Tree
{};


class Num : public Exp
{
public:
  Num (int val)
    : Exp (), val_ (val)
  {}
  virtual void accept (voidVisitor &v) const
  { v.visitNum (*this); };
  virtual int accept (intVisitor &v) const
  { return v.visitNum (*this); };
  int val_;
};

class Var : public Exp
{
public:
  Var (string name)
    : Exp (), name_ (name)
  {}
  virtual ~Var()
  {
    name_.~string();
  }
  virtual void accept (voidVisitor &v) const
  { v.visitVar (*this); };
  virtual int accept (intVisitor &v) const
  { return v.visitVar (*this); };
  string name_;
};

class Bin : public Exp
{
public:
  Bin (char oper, Exp *left, Exp *right)
    : Exp (),
      oper_ (oper),
      left_ (left), right_ (right)
  {}
  virtual ~Bin ()
  {
    delete left_;
    delete right_;
  }
  virtual void accept (voidVisitor &v) const
  {
    v.visitBin (*this);
  }
  virtual int accept (intVisitor &v) const
  {
    return v.visitBin (*this);
  }
  char oper_; Exp *left_; Exp *right_;
};

class App : public Exp
{
public:
  App (string fun, Exp *arg)
    : Exp (),
      fun_ (fun),
      arg_ (arg)
  {}
  virtual ~App ()
  {
    fun_.~string();;
    delete arg_;
  }
  virtual void accept (voidVisitor &v) const
  {
    v.visitApp (*this);
  }
  virtual int accept (intVisitor &v) const
  {
    return v.visitApp (*this);
  }
  string fun_; Exp *arg_;
};

class Ifz : public Exp
{
public:
  Ifz (Exp *cond, Exp *left, Exp *right)
    : Exp (),
      cond_ (cond),
      left_ (left), right_ (right)
  {}
  virtual ~Ifz ()
  {
    delete cond_;
    delete left_;
    delete right_;
  }
  virtual void accept (voidVisitor &v) const
  {
    v.visitIfz (*this);
  }
  virtual int accept (intVisitor &v) const
  {
    return v.visitIfz (*this);
  }
  Exp *cond_; Exp *left_; Exp *right_;
};

/*
 * Declarations
 *
 */

class Decl : public Tree
{
public:
  Decl (string fun, string formal, Exp *body)
    : Tree (),
      fun_(fun), formal_(formal), body_(body)
  {}
  virtual ~Decl ()
  {
    fun_.~string(); formal_.~string();
    delete body_;
  }
  virtual void accept (voidVisitor &v) const
  {
    v.visitDecl (*this);
  }
  virtual int accept (intVisitor &v) const
  {
    cerr << "intVisitor on Decl not implemented\n";
    // return v.visitDecl (*this);
    return 0;
  }
  string fun_; string formal_; Exp *body_;
};

/************************************************************
 * A visitor to print expressions
 ************************************************************/

std::ostream&
operator<< (std::ostream &ostr, const Tree &tree);

class PrintVisitor : public voidVisitor
{
public:
  PrintVisitor (std::ostream &ostr) :
    ostr_ (ostr) {}

  virtual void visitBin (const Bin &exp)
  {
    ostr_ << '(' << *exp.left_
          <<  exp.oper_ << *exp.right_
          << ')';
  }
  virtual void visitApp (const App &exp)
  {
    ostr_ << exp.fun_
          << '(' << *exp.arg_
          << ')';
  }
  virtual void visitIfz (const Ifz &exp)
  {
    ostr_ << "[Ifz " << *exp.cond_
          <<  " then " << *exp.left_
          <<  " else " << *exp.right_
          << ']';
  }
  virtual void visitNum (const Num &exp)
  {
    ostr_ << exp.val_;
  }
  virtual void visitVar (const Var &exp)
  {
    ostr_ << exp.name_;
  }
  virtual void visitDecl (const Decl &decl)
  {
    ostr_ << "{Decl " << decl.fun_
          << "(" << decl.formal_ << ") = "
          << *decl.body_
          << '}';
  }
  std::ostream& ostr_;
};

std::ostream&
operator<< (std::ostream &ostr, const Tree &tree)
{
  PrintVisitor pv (ostr);
  tree.accept (pv);
  return ostr;
}

/************************************************************
 * Environments
 * Arguably it would be better to use a raw map instead of
 * wrapping it with this class.
 ************************************************************/

template <class T>
class Env
{
public:
  void ext(string name, T tree)
  {
    env_[name] = tree;
  }
  T get(string name)
  {
    return env_[name];
  }
  void print(std::ostream &ostr)
  {
    cout << "Env <\n";
    for (typename map<string,T>::iterator i = env_.begin();
	 i != env_.end();
	 i++) {

      cout << i->first << '\t'
	   << *(i->second) << endl;
    }
    cout << ">\n";
  }
  map<string, T> env_;
};

typedef Env<int> IntEnv;
typedef Env<Decl*> DeclEnv;



/******************************************************************
 * The interpreter
 ******************************************************************/
class EvalVisitor : public intVisitor
{
public:
  EvalVisitor (IntEnv &env, DeclEnv &fenv) :
    env_ (env), fenv_ (fenv) {}

  virtual ~EvalVisitor() { }

  virtual int visitBin (const Bin &exp)
  {
    int e1 = exp.left_ -> accept(*this);
    int e2 = exp.right_ -> accept(*this);

    switch(exp.oper_) {
    case '+':	return e1+e2; break;
    case '-':	return e1-e2; break;
    case '*':	return e1*e2; break;
    case '/':	return e1/e2; break;
    default:
      cerr << "Unexpected operand "<< exp.oper_ << "\n";
      return 0;
    }
  }
  virtual int visitApp (const App &exp)
  {
    const Decl *decl = fenv_.get( exp.fun_ );
    IntEnv newEnv(env_);
    int argval = exp.arg_ -> accept(*this);
    newEnv.ext( decl->formal_, argval);
    EvalVisitor ev(newEnv, fenv_);
    return decl->body_ -> accept(ev);
  }
  virtual int visitIfz (const Ifz &exp)
  {
    int c = exp.cond_ -> accept(*this);
    if (c == 0)
      return exp.left_ -> accept(*this);
    else
      return exp.right_ -> accept(*this);
  }
  virtual int visitNum (const Num &exp)
  {
    return exp.val_;
  }
  virtual int visitVar (const Var &exp)
  {
    return env_.get( exp.name_ );
  }
  virtual int visitDecl (const Decl &decl)
  {
    cerr << "Makes no sense to interpret a Decl\n";
  return 0;
  }
  IntEnv env_; DeclEnv fenv_;
};


/******************************************************************
 * The specialiser
 *
 * Generate a taskgraph that delivers the expression result in variable
 * dest.  The function argument is passed in variable arg.
 * The resulting Taskgraph is built onto caller's current taskgraph.
 ******************************************************************/

typedef TaskGraph<int, int> codeIntToInt;

/* First a little trickery to call a taskgraph from taskgraph-generated code
 */

static inline int appCallBackCPPIntToInt(pointer_size_t t, int a) {
  codeIntToInt *T = reinterpret_cast<codeIntToInt *>(t);
  //  cerr << "Callback: ";
  //  T->print();
  int res = T->execute(a);
  return res;
}
extern "C" int appCallbackIntToInt ( pointer_size_t T, int a ) {
  //  fprintf(stderr, "T = %d\n", T);
  int res = appCallBackCPPIntToInt(T, a);
  return res;
}
TaskExpression tExecuteIntToInt ( const TaskExpression &expr1,
				  const TaskExpression &expr2)
{
  static TaskFunction2<int, pointer_size_t, int> func ( "appCallbackIntToInt" );
  return func.call ( expr1, expr2 );
}

/* Now the visitor that walks the tree generating the taskgraph
 */

class SpecVisitor : public voidVisitor
{
public:
  SpecVisitor (IntEnv &env, map<string, codeIntToInt*> &fenv,
	       TaskScalarVariable arg, TaskScalarVariable dest) :
    env_ (env), fenv_ (fenv), arg_ (arg), dest_ (dest) {}

  virtual ~SpecVisitor() { }

  virtual void visitBin (const Bin &exp)
  {
    tVar(int, e1);
    SpecVisitor sv1(env_, fenv_, arg_, e1);
    exp.left_ -> accept(sv1);  // uses new visitor that delivers to e1
    tVar(int, e2);
    SpecVisitor sv2(env_, fenv_, arg_, e2);
    exp.right_ -> accept(sv2); // uses new visitor that delivers to e2

    switch(exp.oper_) {
    case '+':	dest_ = e1+e2; break;
    case '-':	dest_ = e1-e2; break;
    case '*':	dest_ = e1*e2; break;
    case '/':	dest_ = e1/e2; break;
    default:
      cerr << "Unexpected operand "<< exp.oper_ << "\n";
      dest_ = 0;
      }
  }
  virtual void visitApp (const App &exp)
  {
    // Evaluate the argument
    exp.arg_ -> accept(*this); // this visitor delivers result to dest_

    // Find the taskgraph corresponding to this function
    cerr << "Fun is " << exp.fun_ << "\n";
    cerr << "TG is " << fenv_[ exp.fun_ ] << "\n";
    codeIntToInt *body = fenv_[ exp.fun_ ];

    // Now call the taskgraph via an indirect call to a C function in the
    // host code
    dest_ = tExecuteIntToInt( reinterpret_cast<pointer_size_t>(body), dest_ );
  }
  virtual void visitIfz (const Ifz &exp)
  {
    exp.cond_ -> accept(*this); // this visitor delivers result to dest
    tIf (dest_ == 0) {
      exp.left_ -> accept(*this);
    } tElse {
      exp.right_ -> accept(*this);
    }
  }
  virtual void visitNum (const Num &exp)
  {
    dest_ = exp.val_;
  }
  virtual void visitVar (const Var &exp)
  {
    dest_ = arg_; // we assume that the variable is the (one and only) argument
    // dest_ = env_.get( exp.name_ );
  }
  virtual void visitDecl (const Decl &decl)
  {
    cerr << "Makes no sense to specialise a Decl\n";
  }
  IntEnv env_; map<string, codeIntToInt*> fenv_;
  TaskScalarVariable arg_; TaskScalarVariable dest_;
};


/******************************************************************
 * Specialising function declarations
 *
 * For each function declaration in the function environment,
 * create a int->int taskgraph.  Return a map from names
 * to taskgraphs mirroring the interpretive function environment.
 ******************************************************************/

map<string, codeIntToInt*>
specialiseDecls (DeclEnv env)
{
  map<string,codeIntToInt*> codemap;

  map<string, Decl*>::iterator i;
  for(i = env.env_.begin(); i != env.env_.end(); i++)
    {
      IntEnv valenv;
      codeIntToInt *T = new codeIntToInt();
      codemap[i->first] = T;
      taskgraph(codeIntToInt, *T, tuple1(x)) {
	tVar(int, dest);
	SpecVisitor sv(valenv, codemap, x, dest);
	i -> second -> body_ -> accept(sv);
	tReturn(dest);
      }
      //      T->print();
      T->compile( tg::GCC, true );
    }
  return codemap;
}

int nativeFib (int n)
{
  if (n < 2) return 1; else return nativeFib(n-1) + nativeFib(n-2);
}

/************************************************************
 * Main function - test harness
 ************************************************************/

int main() {
#ifdef ENV_CYGWIN
	fprintf(stderr, "This example does not work in cygwin due to linking issues in Windows\n");
	return -1;
#endif

	Decl *fac = new Decl("fac", "x",
		     new Ifz(new Var("x"),
			     new Num(1),
			     new Bin('*', new Var("x"),
				     new App("fac",
					     new Bin('-',new Var("x"),
						         new Num(1))))));

  Decl *fib =
    new Decl("fib", "n",
	     new Ifz(new Var("n"),
		     new Num(1),
		     new Ifz(new Bin('-',new Var("n"),new Num(1)),
			     new Num(1),
			     new Bin('+',
				     new App("fib",new Bin('-',new Var("n"),new Num(1))),
				     new App("fib",new Bin('-',new Var("n"),new Num(2)))))));

  Decl *test = new Decl("test", "x",
			new Num(1000));
  Decl *test2 = new Decl("test2", "x",
			new App("test", new Num(1002)));
  Decl *test3 = new Decl("test3", "x",
			new App("test2", new Num(1003)));

  DeclEnv env;   // create environment containing function definitions
  env.ext("fac", fac);
  env.ext("fib", fib);
  env.ext("test", test);
  env.ext("test2", test2);
  env.ext("test3", test3);

  env.print(cout);

  IntEnv valenv; // empty environment for variables

  EvalVisitor ev(EvalVisitor(valenv, env));

  /************************************************************
   * Test code left for future debugging
   ************************************************************

  cout << "eval: fib(20) = " << (new App("fib",new Num(20))) -> accept(ev) << "\n";

  //  Tree *e = new Ifz(new Num(100), new Num(200), new Num(300));
  //  Tree *e = new Bin('*', new Num(200), new Num(300));
  //  Tree *e = new Bin('*', new Num(200), new Var("x"));

  map<string, codeIntToInt*> emptyCodeMap;
  codeIntToInt T;

  taskgraph(codeIntToInt, T, tuple1(x)) {
    tVar(int, dest);
    SpecVisitor sv(valenv, emptyCodeMap, x, dest);
    e -> accept(sv);
    tReturn(dest);
  }
  T.print();
  T.compile( tg::GCC, true );
  int x = 100;
  cout << "spec: test gives " << T.execute(x) << "\n";
  ************************************************************/

  map<string, codeIntToInt* > codemap = specialiseDecls(env);

  codeIntToInt *t = codemap["fib"];
  t->print();

  for (int i=20; i<32; ++i) {

    // Measure performance of interpreter
    Timer evalTimer;
    int r1 = (new App("fib",new Num(i))) -> accept(ev);
    evalTimer.stop();
    cout << "eval: fib("<<i<<") = " << r1 << " (time: "<<evalTimer.getTime()<<"\n";

    // Measure performance of specialised code
    Timer specTimer;
    int r2 = t->execute(i);
    specTimer.stop();
    cout << "spec: fib("<<i<<") = " << r2 << " (time: "<<specTimer.getTime()<<"\n";

    // Measure performance of native code
    Timer nativeTimer;
    int r3 = nativeFib(i);
    nativeTimer.stop();
    cout << "native: fib("<<i<<") = " << r3 << " (time: "<<nativeTimer.getTime()<<"\n";
  }

  return 0;
}
