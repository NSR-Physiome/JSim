/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Variable memory store options processing (NOT YET IN USE)

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*;
import java.util.*;

public class RTMemory {
	private RTModel model;

	// storeScheme etc. 
	private int storeScheme;  // see ASModel.MEMORY_*
	private boolean[] canQueryVars; // index by var.varID()
	public static final int STATIC = ASModel.MEMORY_STATIC;
	public static final int DYNAMIC = ASModel.MEMORY_DYNAMIC;
	public static final int SELECTED = ASModel.MEMORY_SELECTED;

	// constructor 
	public RTMemory(RTModel model, NamedVal.NList nvals)
	throws Xcept {
	    this.model = model;
	    int nvars = model.vars.size();
	    canQueryVars = new boolean[nvars];
	    for (int i=0; i<model.domains.size(); i++)
	    	addQueryVar((Expr) model.domains.get(i));
	    String s = nvals.stringVal("model.storeExprs", "");
	    StringTokenizer stok = new StringTokenizer(s, ";");
	    while (stok.hasMoreTokens()) 
	    	addQueryExpr(stok.nextToken());
	}	    

	// add selected vars
	public void addQueryExpr(String s) {
	    try {
	    	Expr expr = model.parseExpr(s);
		if (expr instanceof RTVar) {
		    addQueryVar(expr);
		} else {
		    Expr.List vars = new Expr.List();
		    expr.addNamedExpr(vars);
		    for (int i=0; i<vars.size(); i++)
		    	addQueryVar(vars.expr(i));
		}
	    } catch (Xcept e) {
		// ignore
	    }
	}

	public void addQueryVar(Expr expr) {
	    if (canQueryVars != null) {
	    	RTVar v = (RTVar) expr;
	    	canQueryVars[v.varID()] = true;
	    }
	}	    

	// simple query
	public boolean canQueryVar(RTVar v) {
	    if (v.isPrivate()) return false;
	    if (storeScheme == SELECTED) 
	    	return canQueryVars[v.varID()];
	    return true;
	}
	    
	// dump store vars
	public String toString() {
	    if (storeScheme == STATIC) return "memory store: static";
	    if (storeScheme == DYNAMIC) return "memory store: dynamic";
	    StringBuffer buf = new StringBuffer("memory store:");
	    for (int i=0; i<canQueryVars.length; i++) {
	    	if (! canQueryVars[i]) continue; 
		buf.append(" " + model.vars.var(i).name());
	    }
	    return buf.toString();
	}

	// query
//	public int nth(int xid) { return nths[xid]; }
}
