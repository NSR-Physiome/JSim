/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// innate (built-in) expressions

package JSim.expr;
import JSim.util.*;

import java.util.ArrayList;

abstract public class IExpr extends Expr {

	////////////////////////// STATIC VARS & METHODS

	// RealUExpr (1 argument)
	public static final int LN = 3;
	public static final int LOG = 4;
	public static final int SIN = 5;
	public static final int COS = 6;
	public static final int TAN = 7;
	public static final int EXP = 8;
	public static final int SQRT = 9;
	public static final int ASIN = 10;
	public static final int ACOS = 11;
	public static final int SINH = 12;
	public static final int COSH = 13;
	public static final int TANH = 14;
	public static final int ASINH = 15;
	public static final int ACOSH = 16;
	public static final int ATANH = 17;
	public static final int ABS = 18;
	public static final int CEIL = 19;
	public static final int FLOOR = 20;
	public static final int ROUND = 21;
	public static final int BESSELI0 = 22;
	public static final int BESSELI1 = 23;
	public static final int ERF = 24;
	public static final int ERFC = 25;
	private static final int REALUOPMAX = 29;

	// NotExpr
	public static final int NOT = 31;
	private static final int UNOPMAX = 34;

	// RealBExpr (2 arguments)
	public static final int ADD = 35;
	public static final int SUB = 36;
	public static final int MULT = 37;
	public static final int DIV = 38;
	public static final int POW = 39;
	public static final int ATAN = 40;
	public static final int REM = 41;
	public static final int DERIV = 42;
	public static final int BESSELJN = 43;
	public static final int BESSELKN = 44;
	public static final int BESSELYN = 45;
	private static final int REALBOPMAX = 47;

	// CompareExpr
	public static final int EQ = 48;
	public static final int NE = 49;
	public static final int LT = 50;
	public static final int LE = 51;
	public static final int GT = 52;
	public static final int GE = 53;
	public static final int APPROX = 54;
	private static final int COMPAREOPMAX = 57;

	// LogicalExpr
	public static final int AND = 58;
	public static final int OR = 59;
	private static final int LOGICALOPMAX = 62;

	// oddball expr
	public static final int FORALL = 64;
	public static final int IF = 66;

	// zero arg funcs
	private static final int RANDOMMIN = 67;
	public static final int RANDOM = 67; // uniform random
	public static final int RANDOMG = 68; // gaussian random
	public static final int OPMAX = 69;

	// get name
	static String jsname(int i) { return JSLang.lang.chars(i); }

	// number of operators 
	static public int opct(int inx) {
	    if (inx < UNOPMAX) return 1;
	    if (inx == IF) return 3;
	    if (inx >= RANDOMMIN) return 0;
	    return 2;
	}

	// pseudoconstructors
	public static IExpr create(String s, Expr.List elist)
	throws Xcept {
	    int o = JSLang.lang.getOp(s);
	    if (o<=0) throw new Xcept(
		"Unknown function or variable \"" + s + "\"");
	    return create(o, elist);
	}
	public static IExpr create(int op, Expr.List elist)
	throws Xcept {
	    Expr[] args = new Expr[elist.size()];
	    for (int i=0; i<elist.size(); i++)
		args[i] = elist.expr(i);
	    return create(op, args);
	}
	public static IExpr create(int op, Expr[] args) throws Xcept {
	    if (args.length != opct(op)) throw new Xcept(
		"IExpr.create: op " + op + " requires " + 
		opct(op) + " arguments");

	    if (op < REALUOPMAX)
		return new RealUExpr(op, args[0]);
	    else if (op == NOT)
		return new NotExpr(args[0]);
	    else if (op < REALBOPMAX)
		return new RealBExpr(op, args[0], args[1]);
	    else if (op < COMPAREOPMAX)
		return new CompareExpr(op, args[0], args[1]);
	    else if (op < LOGICALOPMAX)
		return new LogicalExpr(op, args[0], args[1]);
	    else if (op == IF)
		return new IfExpr(args[0], args[1], args[2]);
//	    else if (op == IN)
//		return new InExpr(args[0], args[1]);
	    else if (op == FORALL)
		return new ForallExpr(args[0], args[1]);
	    else if (op >= RANDOMMIN)
		return new RandomExpr(op);

	    throw new Xcept("IExpr.create: unknown op " + op);
	}

	////////////////////////// INSTANCE VARS & METHODS
	protected int op;
	protected Expr[] args;	// arguments

	// constructors
	public IExpr(int opp, Expr[] a) throws Xcept {
	    super();
	    op = opp;
	    args = a;
	    if (args.length != opct(op)) throw new Xcept (
		jsname(op) + " requires " + opct(op) + " arguments");
	    for (int i=0; i<args.length; i++) 
		if (args[i] == null) throw new Xcept(this,
		    "Null constructor argument #" + i);
	    Class clss = null;
	    if (op < REALUOPMAX)
		clss = RealUExpr.class;
	    else if (op == NOT)
		clss = NotExpr.class;
	    else if (op < REALBOPMAX)
		clss = RealBExpr.class;
	    else if (op < COMPAREOPMAX)
		clss = CompareExpr.class;
	    else if (op < LOGICALOPMAX)
		clss = LogicalExpr.class;
	    else if (op == IF)
		clss = IfExpr.class;
//	    else if (op == IN)
//		clss = InExpr.class;
	    else if (op == FORALL)
		clss = ForallExpr.class;
	    else if (op >= RANDOMMIN)
		clss = RandomExpr.class;
	    if (this.getClass() != clss)
		throw new Xcept(this, "IExpr class conflict op = " + op);
	}


	// query methods
	public int op() { return op; }
	public int nargs() { return args.length; }
	public Expr arg(int i) { return args[i]; }
	public boolean sameAs(Expr ex) { 
	    if (! IExpr.class.isInstance(ex)) return false;
	    IExpr e = (IExpr) ex;
	    if (op != e.op) return false;
	    for (int i=0; i<args.length; i++) 
		if (!args[i].sameAs(e.args[i])) 
		    return false;
	    return true;
	}
	public boolean isConst() { 
	    for (int i=0; i<args.length; i++) 
		if (!args[i].isConst()) return false;
	    return true;
	}
	public boolean isConstIgnoreUnits() { 
	    for (int i=0; i<args.length; i++) 
		if (!args[i].isConstIgnoreUnits()) return false;
	    return true;
	}
	public boolean needsParen(int argInx, int binop) { return true; }

	// simplify expression
	public Expr simplify() throws Xcept {
	    if (isConst()) return cons();
	    Expr[] nargs = new Expr[args.length];
	    for (int i=0; i<args.length; i++) 
		nargs[i] = args[i].simplify();
	    return create(op, nargs);
	}

	// expand derivatives
	public Expr expandDeriv() throws Xcept {
	    Expr[] nargs = new Expr[args.length];
	    for (int i=0; i<args.length; i++) 
		nargs[i] = args[i].expandDeriv();
	    return create(op, nargs);
	}

	// replace sub-expressions
	public Expr replace(Expr.List list1, Expr.List list2)
	throws Xcept {
	    Expr expr = super.replace(list1, list2);
	    if (expr != this) return expr; 
	    Expr[] nargs = new Expr[args.length];
	    for (int i=0; i<args.length; i++) 
		nargs[i] = args[i].replace(list1, list2);
	    return create(op, nargs);
	}	    

	// lists
	public void addNamedExpr(Expr.List list) throws Xcept {
	    for (int i=0; i<args.length; i++)
		args[i].addNamedExpr(list);
	}	    
	public void addDomains(Expr.List list) {
	    for (int i=0; i<args.length; i++)
		args[i].addDomains(list);
	}

	// string rep
	public String toString() {
	    return toString(JSLang.lang, null);
	}
	public String toString(Context ctxt) {
	    return toString(ctxt.lang, ctxt);
	}
	public String toString(Lang lang, Context ctxt) {

	    // swap args (atan in java, others?)
	    Expr[] sargs = args;
	    if (lang.swapXY(op) && sargs.length == 2) {
		sargs = new Expr[2];
		sargs[0] = args[1];
		sargs[1] = args[0];
	    }

	    // algebraic format
	    String ops = lang.chars(op);
	    if (! lang.func(op)) {
		Expr a = sargs[0];
		Expr b = sargs[1];
		String as = (ctxt == null) ? a.toString() : a.toString(ctxt);
		if (a.needsParen(0, op)) as = "(" + as + ")";
		String bs = (ctxt == null) ? b.toString() : b.toString(ctxt);
		if (b.needsParen(1, op)) bs = "(" + bs + ")";
		as = lang.castString(a, as);
		bs = lang.castString(b, bs);
		if (Character.isLetter(ops.charAt(0)))
		    ops = " " + ops + " ";
		return as + ops + bs;
	    }

	    // functional format
	    String s = ops + "(";
	    for (int i=0; i<sargs.length; i++) {
		if (i>0) s = s + ",";
		Expr a = sargs[i];
		String as = (ctxt == null) ? a.toString() : a.toString(ctxt);
		as = lang.castString(a, as);
		s = s + as;
	    }
	    return s + ")";
	}

	// unary incompatible units
	protected void checkDimless(Expr a, Unit au) 
	throws Xcept {
	    if (Unit.compatible(au,Unit.scalar())) return;
	    throw new UnitXcept(this, a, null, au, null);
	}

	// binary incompatible units
	protected void sameDim(Expr a, Expr b, Unit au, Unit bu) 
	throws Xcept {
	    if (Unit.compatible(au,bu)) return;
	    throw new UnitXcept(this, a, b, au, bu);
	}
}


