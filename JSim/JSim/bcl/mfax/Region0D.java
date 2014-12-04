/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 0D region of MFAX system (stirred tank)

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Region0D extends Region {

	// constructor
	public Region0D(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	}

	// add variables for 1 chemical
	protected void addChem(Chem c) throws Xcept {
	    addConc(c);
	}

	// solve flows
	protected void solve1() throws Xcept {
	    super.solve1();

	    if (inflows.size() != outflows.size()) throw new Xcept(
		name() + ": unbalanced region inflow & outflow");
	    if (inflows.size() > 1) throw new Xcept(
		name() + ": overconnected region inflow & outflow");
	    if (inflows.size() > 0) {
	    	Flow inflow = (Flow) inflows.comp(0);
	    	Flow outflow = (Flow) outflows.comp(0);
	   	outflow.setInflow(inflow.flow);
	    }
	}

	// slow delta changes
	protected Expr slowDelta(Chem c) throws Xcept {
	    Expr e = Expr.zero;
		
	    // inflow term(s?)
	    for (int j=0; j<inflows.size(); j++) {
		Flow flow = (Flow) inflows.comp(j);
		e = e.add(flow.flow.div(vol).mult(flow.conc(c)));
	    }

	    // outflow term(s?)
	    for (int j=0; j<outflows.size(); j++) {
		Flow flow = (Flow) outflows.comp(j);
		e = e.sub(flow.flow.div(vol).mult(conc(c)));
	     }

	    // add production/consumption terms
	    for (int j=0; j<prod.size(); j++) {
		Production p = prod.prod(j);
		e = e.add(p.concDelta(c));
	    }

	    // add slow reaction terms
	    for (int j=0; j<reac.size(); j++) {
		if (reac.reac(j).isFast()) continue;
		ReactionSlow r = (ReactionSlow) reac.reac(j);
		e = e.add(r.concDelta(c));
	    }

	    // add transport terms
	    for (int j=0; j<mem.size(); j++) {
		Membrane m = (Membrane) mem.comp(j);
		for (int k=0; k<m.transp.size(); k++) {
		    Transport tr = (Transport) m.transp.comp(k);
		    e = e.add(tr.concDelta(this,c));
		}
	    }

	    return e.simplify();
	}

}
