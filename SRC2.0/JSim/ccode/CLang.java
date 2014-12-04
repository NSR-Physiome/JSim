/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// C language text generator

package JSim.ccode;

import JSim.util.*;
import JSim.expr.*;

public class CLang extends Lang {

	// static stuff
	static public Lang lang;
	static { lang = new CLang();	}

	// constructor
	private CLang() {
	    super("__", "__JSD");
	    addop(IExpr.LOG, "log10");
	    addop(IExpr.SIN, "sin");
	    addop(IExpr.COS, "cos");
	    addop(IExpr.TAN, "tan");
	    addop(IExpr.EXP, "exp");
	    addop(IExpr.SQRT, "sqrt");
	    addop(IExpr.ASIN, "asin");
	    addop(IExpr.ACOS, "acos");
	    addop(IExpr.ABS, "fabs");
	    addop(IExpr.CEIL, "ceil");
	    addop(IExpr.FLOOR, "floor");
	    addop(IExpr.ROUND, "round");
	    addop(IExpr.ADD, "+");
	    addop(IExpr.SUB, "-");
	    addop(IExpr.MULT, "*");
	    addop(IExpr.DIV, "/");
	    addop(IExpr.POW, "pow");
	    addop(IExpr.DERIV, ":");
	    addop(IExpr.ATAN, "atan2");
	    addop(IExpr.EQ, "==");
	    addop(IExpr.NE, "!=");
	    addop(IExpr.LT, "<");
	    addop(IExpr.LE, "<=");
	    addop(IExpr.GT, ">");
	    addop(IExpr.GE, ">=");
	    addop(IExpr.AND, "&&");
	    addop(IExpr.OR, "||");
	    addop(IExpr.IF, "?");
	}


	// cast one type to another - needs work
	public String castString(Expr a, String as) {
//	    Var v = a.getVar();
//	    if (v != null && v.isInt())
//		return "((double) " + as + ")";
	    return as;
	}

	// if stmt
	public String ifStr(String as, String bs, String cs) {
	    return "(" + as + ") ? (" + bs + ") : (" + cs + ")";
	
	}

	// swap 1st & 2nd arguments for toString (e.g. atan)
	public boolean swapXY(int op) { 
	     return op == IExpr.ATAN; 
	}
}
