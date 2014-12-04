/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// single variable calculated via TExpr

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ExprTool extends Tool {
	public VarUsage vu;   // single VarUsage solved
	public TExpr expr; // expr to calculate
	
	// constructor
	public ExprTool(VarUsage vu, TExpr expr) throws Xcept {
	    super(vu.model());
	    this.vu = vu;
	    this.expr = expr;
	    vsols.add(vu);
	    vreqs.add(expr.usages());
	}

	// deriv tool
	public Tool derivTool(Domain x) throws Xcept {
	    VarUsage vux = vu.deriv(x);
	    if (vux == null) return null;
	    TExpr exprx = expr.deriv(x);
	    return new ExprTool(vux, exprx);
	} 

	// query
	public Var v() { return vu.v(); }
	public String toString() { return "" + vu + "=" + expr; }
	public String toolType() { return "expr"; }
}
