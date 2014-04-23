/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Java language text generator

package JSim.jcode;

import JSim.util.*;
import JSim.expr.*;
import JSim.nml.math.NatMath;

public class JLang extends Lang {

	// static stuff
	static public Lang lang;
	static { lang = new JLang();	}

	// constructor
	private JLang() {
	    super("__", "__JSD");
	    addop(IExpr.LN, "NatMath.log");
	    addop(IExpr.LOG, "NatMath.log10");
	    addop(IExpr.SIN, "Math.sin"); // linux/macos differ
	    addop(IExpr.COS, "Math.cos"); // ditto
	    addop(IExpr.TAN, "NatMath.tan");
	    addop(IExpr.EXP, "NatMath.exp");
	    addop(IExpr.SQRT, "Math.sqrt"); // java faster than native
	    addop(IExpr.ASIN, "NatMath.asin");
	    addop(IExpr.ACOS, "NatMath.acos");
	    addop(IExpr.SINH, "NatMath.sinh");
	    addop(IExpr.COSH, "NatMath.cosh");
	    addop(IExpr.TANH, "UtilMath.tanh");
	    addop(IExpr.ASINH, "UtilMath.asinh");
	    addop(IExpr.ACOSH, "UtilMath.acosh");
	    addop(IExpr.ATANH, "UtilMath.atanh");
	    addop(IExpr.ABS, "Math.abs");
	    addop(IExpr.CEIL, "Math.ceil");
	    addop(IExpr.FLOOR, "Math.floor");
	    addop(IExpr.ROUND, "Math.round");
	    addop(IExpr.BESSELI0, "cern.jet.math.Bessel.i0");
	    addop(IExpr.BESSELI1, "cern.jet.math.Bessel.i1");
	    addop(IExpr.BESSELJN, "UtilMath.besselJn");
	    addop(IExpr.BESSELKN, "UtilMath.besselKn");
	    addop(IExpr.BESSELYN, "UtilMath.besselYn");
	    addop(IExpr.ERF,  "cern.jet.stat.Probability.errorFunction");
	    addop(IExpr.ERFC, "cern.jet.stat.Probability.errorFunctionComplemented");
	    addop(IExpr.ADD, "+");
	    addop(IExpr.SUB, "-");
	    addop(IExpr.MULT, "*");
	    addop(IExpr.DIV, "/");
	    addop(IExpr.POW, "NatMath.pow");
	    addop(IExpr.DERIV, ":");
	    addop(IExpr.ATAN, "NatMath.atan2");
	    addop(IExpr.REM, "UtilMath.rem");
	    addop(IExpr.EQ, "==");
	    addop(IExpr.NE, "!=");
	    addop(IExpr.APPROX, "~=");
	    addop(IExpr.LT, "<");
	    addop(IExpr.LE, "<=");
	    addop(IExpr.GT, ">");
	    addop(IExpr.GE, ">=");
	    addop(IExpr.AND, "&&");
	    addop(IExpr.OR, "||");
	    addop(IExpr.FORALL, "@"); // bogus, but helps debugging
	    addop(IExpr.IF, "?", false);
	    addop(IExpr.RANDOM, "random");
	    addop(IExpr.RANDOMG, "randomg");
	}

	// if stmt
	public String ifStr(String as, String bs, String cs) {
	    return "(" + as + ") ? (" + bs + ") : (" + cs + ")";
	
	}

	// swap 1st & 2nd arguments for toString (e.g. atan)
	public boolean swapXY(int op) { 
	     return op == IExpr.ATAN; 
	}

	// indent # spaces
	public int indent(int i) { 
	    if (i==0) return 0;
	    return 4 + 4*i; 
	}
}
