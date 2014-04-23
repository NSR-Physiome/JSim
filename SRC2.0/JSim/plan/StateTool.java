/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// state variable (retains init/curr value until changed by event)

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class StateTool extends Tool {
	public Var v;   // variable solved
	public Domain t; // time domain
	
	// constructor
	public StateTool(TModel model, Var v) throws Xcept {
	    super(model);
	    this.v = v;
	    vsols.add(v);
	    if (v.ndim() == 1)
	    	setT(v.domain(0));
	    icTools = new LinkedHashSet<Tool>();
	}

	// set ictool
	public void setIC(Tool tool) throws Xcept {
	    if (icTools.contains(tool)) return;
	    if (icTools.size() > 0) throw new Xcept(
		"Extern variable " + v + 
		" has conflicting ICs: " + tool +
		" and " + icTools);
	    icTools.add(tool);;
	    VarUsages vus = tool.vsols;
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (vu.v() != v) continue;
		if (vu.stat() != VarUsage.MIN) continue;
		setT(vu.domain());
	    }
	}	

	// set t domain
	public void setT(Domain t) throws Xcept {
	    if (this.t == null)
	        this.t = t;
	    if (this.t != t) throw new Xcept(
	    	"Ambiguous time domain for state variable " + v +
		" (" + t + ", " + this.t + ")");
	    vreqs.add(new VarUsage(model, v, VarUsage.MIN, t));
	}
	    	
	// deriv tool
	public Tool derivTool(Domain x) throws Xcept {
	    Var vx = v.deriv(x);
	    VarUsage vux = new VarUsage(model, vx);
	    TExpr zerox = new TExpr(model, Expr.zero);
	    return new ExprTool(vux, zerox);
	}

	// query
	public Var v() { return v; }
	public Domain t() { return t; }
	public String toString() { return "state " + v; }
	protected boolean needsICs() { return true; }
	protected boolean isComplete() { return icTools.size() > 0; }
	protected LinkedHashSet<Tool> icTools() { return icTools; }
	public String toolType() { return "state"; }
}
