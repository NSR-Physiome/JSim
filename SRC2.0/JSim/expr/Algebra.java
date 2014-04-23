/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.expr;
import JSim.util.*;

import java.util.*;
import java.io.*;
import java.net.URL;

public class Algebra {

	// solve lhs=rhs for x,  return expr for x
	//    throw Xcept if can't with reason why not
	public static Expr solve(NamedExpr x, Expr lhs, Expr rhs) throws Xcept {
	    return solve(x, lhs, rhs, false);
	}

	// solve enough to determine x's units
	public static Expr solveUnit(NamedExpr x, Expr lhs, Expr rhs) throws Xcept {
	    return solve(x, lhs, rhs, true);
	}

	private static Expr solve(NamedExpr x, Expr lhs, Expr rhs, boolean unit) 
	throws Xcept {
	    int ct = 0;

	    // loop until return or Xcept
	    while(true) {
		if (ct++>100) throw new Xcept(
		    "Algebra.solve() nesting overflow,  probably bug");

		// make sure lhs has x,  rhs does not
		Expr.List llist = new NamedExpr.List(4);
		lhs.addNamedExpr(llist);
		boolean lx = llist.containSame(x);
		Expr.List rlist = new NamedExpr.List(4);
		rhs.addNamedExpr(rlist);
		boolean rx = rlist.containSame(x);
		if (lx && rx) throw new Xcept(x, 
		    "occurs multiple times in equation");
		if (!lx && !rx) throw new Xcept(x, 
		    "occurs nowhere in equation");
		if (rx) {
		    Expr temp = rhs;
		    rhs = lhs;
		    lhs = temp;
		}

		// lhs=x,  done
		if (lhs.sameAs(x)) return rhs;

		// binary expression?
		if (lhs instanceof RealBExpr) {
		    RealBExpr e = (RealBExpr) lhs;
		    Expr e1 = e.arg(0);
		    Expr e2 = e.arg(1);
		    Expr.List list1 = new NamedExpr.List(4);
		    e1.addNamedExpr(list1);
		    boolean in1 = list1.containSame(x);
		    Expr.List list2 = new NamedExpr.List(4);
		    e2.addNamedExpr(list2);
		    boolean in2 = list2.containSame(x);
		    if (in1 && in2) throw new Xcept(x, 
		    	"occurs multiple times in binary expr");
		    if (!in1 && !in2) throw new Xcept(x,
		    	"occurs nowhere in binary expr");

		    // calculate new lhs and rhs
		    Expr a = in1 ? e2 : e1;
		    Expr b = rhs;
		    Expr y = null;
		    switch(e.op()) {
		    // y+a=b -> y=b-a  a+y=b -> y=b-a
		    case IExpr.ADD:
			y = b.sub(a);
			break;

		    // y-a=b -> y=b+a  a-y=b -> y=a-b
		    case IExpr.SUB:
			y = in1 ? b.add(a) : a.sub(b);
			break;

		    // y*a=b -> y=b/a  a*y=b -> y=b/a
		    case IExpr.MULT:
			y = b.div(a);
			break;

		    // y/a=b -> y=a*b  a/y=b -> y=a/b
		    case IExpr.DIV:
			y = in1 ? a.mult(b) : a.div(b);
			break;

		    // powers
		    case IExpr.POW:
			// y^a=b -> y=b^(1/a) (units only)
			// otherwise special cases for odd integers
			//           and inverse integers (later)
			if (in1) {
			    if (unit) y = b.pow(Expr.one.div(a));

		        // a^y=b -> y=ln(b)/ln(a)
			} else {
			    Expr lna = new RealUExpr(IExpr.LN, a);
			    Expr lnb = new RealUExpr(IExpr.LN, b);
			    y = lnb.div(lna);
			}
			break;
		    }

		    if (y == null) throw new Xcept(e,
			"Uninvertible binary operation");
		    lhs = in1 ? e1 : e2;
		    rhs = y;

		// unary operations
		} else if (lhs instanceof RealUExpr) {
		    RealUExpr e = (RealUExpr) lhs;
		    Expr a = rhs;
		    Expr y = null;
		    switch (e.op()) {

		    // ln(y)=a -> y=exp(a)
		    case IExpr.LN:
			y = new RealUExpr(IExpr.EXP, a);
			break;

		    // log(y)=a -> y=10^a 
		    case IExpr.LOG:
			y = Expr.cons(10).pow(a);
			break;

		    // exp(y)=a -> y=ln(a)
		    case IExpr.EXP:
			y = new RealUExpr(IExpr.LN, a);
			break;

		    // sqrt(y)=a -> y=a^2
		    case IExpr.SQRT:
			y = a.pow(2);
			break;

		    // unit invertable f(y)=a -> y=a
		    case IExpr.ABS:
		    case IExpr.CEIL:
		    case IExpr.FLOOR:
		    case IExpr.ROUND:
			if (unit) y = a;
			break;

		    default:
			throw new Xcept(e,
			    "Uninvertible unary operation");
		    }
		    if (y == null) throw new Xcept(e,
			"Uninvertible unary operation");
		    lhs = e.arg(0);
		    rhs = y;

		// guess not
		} else throw new Xcept(lhs,
		    "Uninvertible expression");
	    }
	}
			    
}

