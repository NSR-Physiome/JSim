/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// solves Domain var, possibly control variables as well

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class DomainTool extends Tool {
	public Domain x;   // domain solved
	private boolean[] auxKnown; // which aux's are known
	private int nauxKnown;
	
	// constructor
	public DomainTool(TModel model, Var v) throws Xcept {
	    super(model);
	    x = (Domain) v;
	    vsols.add(x);
	    vreqs.add(x.vmin);
	    vreqs.add(x.vmax);
	    vreqs.add(x.vct);
	    vreqs.add(x.vdelta);
	    auxKnown = new boolean[Domain.NAUX];
	    nauxKnown = 0;
	}

	// add aux to solved list, if 3 return 4th tool
	protected ExprTool addAux(Var xaux) throws Xcept {
	    int inx = x.auxInx(xaux);
	    if (auxKnown[inx]) return null;
	    auxKnown[inx] = true;
	    nauxKnown++;
	    if (nauxKnown != 3) return null;

	    // 3 vaux known, create 4th vaux tool
	    inx = -1;  // crash if auxKnown messed up
	    for (int i=0; i<Domain.NAUX; i++)
	    	if (! auxKnown[i]) inx = i;
	    VarUsage vu = new VarUsage(model, x.vaux(inx));
	    Expr expr = null;
	    switch (inx) {
	    case Domain.MIN:
	    	expr = x.vmax.sub(x.vdelta.mult(x.vct.sub(Expr.one)));
		break;
	    case Domain.MAX:
	    	expr = x.vmin.add(x.vdelta.mult(x.vct.sub(Expr.one)));
		break;
	    case Domain.CT:
	    	expr = x.vmax.sub(x.vmin).div(x.vdelta).add(Expr.cons(1.5));
		break;
	    case Domain.DELTA:
	        expr = x.vmax.sub(x.vmin).div(x.vct.sub(Expr.one));
		break;
	    }
	    TExpr texpr = new TExpr(model, expr);
	    return new ExprTool(vu, texpr);
	}

	// set sequence loops field
	protected void setSeqLoops() throws Xcept {
	    seqLoops = new DomainSet();
	}

	// simple query
	public Domain x() { return x; }
	public String toString() { return "domain " + x; }
	public String toolType() { return "domain"; }
}
