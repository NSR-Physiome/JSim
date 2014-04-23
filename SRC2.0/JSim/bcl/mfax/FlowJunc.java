/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Junction of 2 or more 

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class FlowJunc extends FlowComp {
	Expr totflow;

	// constructor
	public FlowJunc(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e, 0);
	    for (int i=0; i<sys.chem.size(); i++) 
		addChem(sys.chem.chem(i));
   	}

	// add chemical internal variables
	protected void addChem(Chem c) throws Xcept {
	    addConc(c);
	}

	// attach Flow
	protected void attach(boolean isin, Flow f) throws Xcept {
	    super.attach(isin,f);
	    if (!isin)
	        new RealNVar(this, f.name() + "wgt", sys.tlist());
	}

	// get weight
	protected RealNVar wgt(Flow f) throws Xcept {
	    Comp c = getChild(f.name() + "wgt");
	    if (c == null) throw new Xcept(this, "missing weight");
	    return (RealNVar) c;
	}

	// solve flows
	protected void solve1() throws Xcept {
	    super.solve1();

	    if (inflows.size() == 0 && outflows.size() == 0) 
		return;
	    if (inflows.size() == 0) 
		throw new Xcept(this, "no inflow");
	    if (outflows.size() == 0) 
		throw new Xcept(this, "no outflow");
	
	    // total inflow and wgts
	    totflow = Expr.zero;
	    for (int i=0; i<inflows.size(); i++) {
		Flow f = (Flow) inflows.comp(i);
		totflow = totflow.add(f.flow);
	    }	
	    Expr totwgt = Expr.zero;
	    for (int i=0; i<outflows.size(); i++) {
		Flow f = (Flow) outflows.comp(i);
		totwgt = totwgt.add(wgt(f));
		if (outflows.size() == 1) 
		    wgt(f).setAccess(Comp.PRIVATE);
	    }

	    // set outflows 
	    for (int i=0; i<outflows.size(); i++) {
		Flow f = (Flow) outflows.comp(i);
		Expr e = totflow.mult(wgt(f).div(totwgt));	    	
		f.setInflow(e);
	    }
	}
	
	// solve conc
	protected void solve2() throws Xcept {
	    super.solve2();

	    // set concentrations
	    if (inflows.size() == 0) return;
	    for (int i=0; i<sys.chem.size(); i++) {
		Chem c = sys.chem.chem(i);
		Expr totamt = Expr.zero;
	    	for (int j=0; j<inflows.size(); j++) {
		    Flow f = (Flow) inflows.comp(j);
	    	    totamt = totamt.add(f.flow.mult(f.conc(c)));
		}
		setVar(conc(c), totamt.div(totflow), true);
	    }

	    // default weights
	    for (int i=0; i<outflows.size(); i++) {
		Flow f = (Flow) outflows.comp(i);
		setVar(wgt(f), Expr.one, false);
	    }
	}
	
	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();

	    // weights scalar
	    for (int i=0; i<outflows.size(); i++) {
		Flow f = (Flow) outflows.comp(i);
		setVarUnit(wgt(f), Unit.scalar());
	    }

	    // chem concentrations mole/liter
	    assignConcUnits();
	}
}
