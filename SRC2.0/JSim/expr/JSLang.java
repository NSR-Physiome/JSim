/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim language text generator

package JSim.expr;
import JSim.util.*;

public class JSLang extends Lang {

	// static stuff
	static public Lang lang;
	static { lang = new JSLang();	}

	// constructor
	private JSLang() {
	    super(".", ":");
	    addop(IExpr.LN, "ln");
	    addop(IExpr.LOG, "log");
	    addop(IExpr.SIN, "sin");
	    addop(IExpr.COS, "cos");
	    addop(IExpr.TAN, "tan");
	    addop(IExpr.EXP, "exp");
	    addop(IExpr.SQRT, "sqrt");
	    addop(IExpr.ASIN, "asin");
	    addop(IExpr.ACOS, "acos");
	    addop(IExpr.SINH, "sinh");
	    addop(IExpr.COSH, "cosh");
	    addop(IExpr.TANH, "tanh");
	    addop(IExpr.ASINH, "asinh");
	    addop(IExpr.ACOSH, "acosh");
	    addop(IExpr.ATANH, "atanh");
	    addop(IExpr.ABS, "abs");
	    addop(IExpr.CEIL, "ceil");
	    addop(IExpr.FLOOR, "floor");
	    addop(IExpr.ROUND, "round");
	    addop(IExpr.BESSELI0, "besseli0");
	    addop(IExpr.BESSELI1, "besseli1");
	    addop(IExpr.ERF, "erf");
	    addop(IExpr.ERFC, "erfc");
	    addop(IExpr.NOT, "not");
	    addop(IExpr.ADD, "+");
	    addop(IExpr.SUB, "-");
	    addop(IExpr.MULT, "*");
	    addop(IExpr.DIV, "/");
	    addop(IExpr.POW, "^");
	    addop(IExpr.DERIV, ":");
	    addop(IExpr.BESSELJN, "besseljn");
	    addop(IExpr.BESSELKN, "besselkn");
	    addop(IExpr.BESSELYN, "besselyn");
	    addop(IExpr.ATAN, "atan");
	    addop(IExpr.REM, "rem");
	    addop(IExpr.EQ, "=");
	    addop(IExpr.NE, "<>");
	    addop(IExpr.APPROX, "~=");
	    addop(IExpr.LT, "<");
	    addop(IExpr.LE, "<=");
	    addop(IExpr.GT, ">");
	    addop(IExpr.GE, ">=");
	    addop(IExpr.AND, "and", false);
	    addop(IExpr.OR, "or", false);
	    addop(IExpr.FORALL, "@", false);
	    addop(IExpr.IF, "if");
	    addop(IExpr.RANDOM, "random");
	    addop(IExpr.RANDOMG, "randomg");
	}

	// if stmt
	public String ifStr(String as, String bs, String cs) {
	    return "if (" + as + ") " + bs + " else " + cs;
	}

	// swap 1st & 2nd arguments for toString (e.g. atan)
	public boolean swapXY(int op) { return false; }

}
