/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// generalized Flow

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

abstract public class Flow extends MFComp {
	FlowComp incomp, outcomp; // connected components
	RealNVar flow;
	Inject.List inject;
	Expr srcFin;	// (possibly unbalanced) input flow for FlowSource

	// constructor
	public Flow(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    flow = new RealNVar(this, "flow", sys.tlist());
	    inject = new Inject.List(1);
	    for (int i=0; i<sys.chem.size(); i++)
		addChem(sys.chem.chem(i));
 	}

	// add chemical internal variables
	protected void addChem(Chem c) throws Xcept {
	    addConc(c);
	}

	// attach Junction
	protected void attach(boolean isin, FlowComp fc)
	throws Xcept {
	    if (isin) incomp = fc; else outcomp = fc;
	    fc.attach(!isin, this);
	}

	// attach Inject
	protected void attach(Inject i) {
	    inject.add(i);
	}

	// set inflow
	protected void setInflow(Expr fx) throws Xcept {
	    if (getClass() == FlowSource.class) srcFin = fx;
	    else setVar(flow, fx, true);
	}

	// solve conc
	protected void solve2() throws Xcept {
	    super.solve2();

	    // solve chem concentrations
	    for (int i=0; i<sys.chem.size(); i++) {
		Chem c = sys.chem.chem(i);
		Expr e = Expr.zero;
		if (incomp != null) {
		    e = incomp.conc(c);
		    if (srcFin != null)
			e = e.mult(srcFin.div(flow));
		}
		for (int j=0; j<inject.size(); j++) {
		    Inject inj = (Inject) inject.comp(j);
		    if (inj.chem != c) continue;
		    e = e.add(inj.flux.div(flow));
		}
	        setVar(conc(c), e, true);
	    }
	}

	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    assignConcUnits();
	}

        // Flow.List
        public static class List extends Comp.List {
          public List(int n) {
            super(n);
          }
        }
}
