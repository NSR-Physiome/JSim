/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// vars calc'ed via procedure

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ProcTool extends Tool {
	private XFuncCall fc; // procedure call
	private TSubDom sdom; // when clause
	
	// constructor
	public ProcTool(TModel model, XFuncCall fc) 
	throws Xcept {
	    super(model);
	    this.fc = fc;
	    SubDom sd = new SubDom(fc.voidSD());
	    sdom = new TSubDom(model, sd);

	    // accumulate vsols, vreqs
	    for (int i=0; i<fc.args().size(); i++) {
	        XFuncArg arg = fc.args().get(i);
		boolean isInput = i<fc.ninputs();
		if (isInput) {
		    TExpr bexpr = new TExpr(model, arg.orig());
		    vreqs.add(bexpr.usages());
		} else {
		    VarUsage vu = vsol(arg.base());
		    if (vu == null) throw new Xcept(
		    	"Procedure call " + fc + 
			": illegal output argument #" + (i+1));
		    vsols.add(vu);
		} 
		    
	    }    
	    if (! sdom.isEntire()) {
	    	vreqs = vreqs.restrict(sdom);
	    	vsols = vsols.restrict(sdom);
	    }
	}

	// output arg usage
	private VarUsage vsol(Expr expr) throws Xcept {
	    if (expr instanceof Var) 
	        return new VarUsage(model, (Var) expr);
	
  	    // check for v(t.min) or v(t.max)
	    if (! (expr instanceof VarFuncCall)) return null;
	    VarFuncCall vfc = (VarFuncCall) expr;
	    Var v = vfc.v;
	    Domain t = null;
	    Var taux = null;
	    for (int i=0; i<v.ndim(); i++) {
		if (! (vfc.args.get(i) instanceof Var)) return null;
		Var vaux = (Var) vfc.args.get(i);
	    	Domain x = v.domain(i);
		if (vaux == x) continue;
		if (vaux != x.vmin && vaux != x.vmax) return null;
		if (taux != null) return null;
		t = x;
		taux = vaux;
	    }
	    if (taux == null) return null;
	    int stat = (taux == t.vmin) ? MIN : MAX;
	    return new VarUsage(model, v, stat, t);
	}
	
	// set seq loops: modified from Tool using argDoms
	protected void setSeqLoops() throws Xcept {
	    XFuncArg arg0 = fc.args().get(fc.ninputs());
	    seqLoops = seqLoops(arg0);
	    for (int i=0; i<fc.args().size(); i++) {
	        XFuncArg arg = fc.args().get(i);
		DomainSet loops = seqLoops(arg);
		boolean isInput = i<fc.ninputs();
		boolean ok = true;		
		if (!isInput && !loops.equals(seqLoops)) 
		    ok = false;
		if (isInput && !seqLoops.containsAll(loops)) 
		    ok = false;
		if (ok) continue;
		throw new AbortXcept("Procedure call " + fc +
		    " arguments " + arg0 + " and " + arg +
		    " require incompatible domain loops");
	    }
	}

	// calc seqLoops for a XFuncArg
	private DomainSet seqLoops(XFuncArg arg) throws Xcept {
	    TExpr bexpr = new TExpr(model, arg.base());
	    bexpr = bexpr.restrict(sdom);
	    DomainSet loops = bexpr.usages().seqLoops();
	    loops.removeAll(arg.argDoms());
	    return loops;
	}

	// query
	public String toString() { return "procedure " + fc; }
	public String toolType() { return "procedure"; }
	public XFuncCall fc() { return fc; }
	public TSubDom sdom() { return sdom; }
}
