/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// physical region of biological system

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

abstract public class Region extends FlowComp {
	protected RealNVar vol;
	Membrane.List mem;	// attached membranes
	Production.List prod;	// attached production/consumption
	Reaction.List reac;	// attached reactions
	FastGroup.List fastGroups; // fast equilibration reaction groups
	Chem.List fastChem;	// chems in fast reactions

	// constructor
	public Region(Comp c, String n, Expr.List e) throws Xcept {
	    super(c, n, e);
	    mem = new Membrane.List(1);
	    prod = new Production.List(1);
	    reac = new Reaction.List(1);
	    vol = new RealNVar(this, "vol", null);
	    checkNParm(e, 0);
	    for (int i=0; i<sys.chem.size(); i++) 
		addChem(sys.chem.chem(i));
 	}

	// attach Membrane  to region
	protected void attach(Membrane m) {
	    mem.add(m);
	}

	// attach Production  to region
	protected void attach(Production p) {
	    prod.add(p);
	}

	// attach Reaction  to region
	protected void attach(Reaction q) {
	    reac.add(q);
	}

	// solve concentrations
	protected void solve2() throws Xcept {
	    super.solve2();

	    // create fast reaction groups
	    fastGroups = new FastGroup.List(4);
	    fastChem = new Chem.List(4);
	    boolean[] rfast = new boolean[reac.size()]; 
	    for (int i=0; i<rfast.length; i++) 
		rfast[i] = reac.reac(i).isFast();
	    for (int i=0; i<rfast.length; i++) { 
		if (!rfast[i]) continue;
		ReactionFast wr = (ReactionFast) reac.reac(i);
		FastGroup wgroup = new FastGroup(this, wr);	
		rfast[i] = false;
		boolean working = true;
		while (working) {
		    working = false;
		    for (int j=i+1; j<rfast.length; j++) {
		    	if (!rfast[j]) continue;
		    	wr = (ReactionFast) reac.reac(j);
			if (! wgroup.chem.xsects(wr.eqn.chem)) continue;
			wgroup.add(wr);
			rfast[j] = false;
			working = true;
		    }
		}
	    }

	    // write slow reaction eqns
	    for (int i=0; i<sys.chem.size(); i++) {
		Chem c = sys.chem.chem(i);
		if (isFast(c)) continue;
		Var v = conc(c);
		if (! varSet(SubDom.entire(), v)) {
		    setVar(sys.t.lhbc(), v, Expr.zero, false);
		    Expr e = slowDelta(c);
		    setVar(v.deriv(sys.t), e, false);
		}
	    }

	    // write fast reaction eqns
	    for (int i=0; i<fastGroups.size(); i++) {
		fastGroups.group(i).solve2();
	    }

	    // enforce all conc >= 0
	    // disabled,  bug in planner code
//	    for (int i=0; i<sys.chem.size(); i++) {
//		Chem c = sys.chem.chem(i);
//		Eqn eqn = new Eqn(sys, Expr.truex,
//		    conc(c), IExpr.GE, Expr.zero);
//	    }

	}

	// slow delta
	abstract protected Expr slowDelta(Chem c) throws Xcept;

	// query
	protected boolean isFast(Chem c) { return fastChem.contains(c); }
	protected Expr uClamp(Chem c) throws Xcept {
	    return varExpr(SubDom.entire(), conc(c));
	}
	protected Expr utClamp(Chem c) throws Xcept {
	    return varExpr(SubDom.entire(), conc(c).deriv(sys.t));
	}

	// compatible unit
	public String compatibleUnit() { return "liter"; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    setVarUnit(vol, unit);
	    assignConcUnits();
	}
}
