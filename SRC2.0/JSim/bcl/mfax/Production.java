/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// production of chemical in region

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Production extends MFComp {
	Region region;
	Chem chem;
	RealNVar flux;

	// constructor
	public Production(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e,2);
	    region = (Region) getParm(e,0,Region.class);
	    chem = (Chem) getParm(e,1,Chem.class);
	    flux = new RealNVar(this, "flux", sys.tlist());
	    region.attach(this);
	}

	// delta concentration
	public Expr concDelta(Chem ch) throws Xcept {
	    return (chem == ch) ? flux.div(region.vol) : Expr.zero;
	}

	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    setVarUnit(flux, chem.fluxUnit());
	}

 	// solve concentrations
	protected void solve2() throws Xcept {
	    super.solve2();
	    setVar(flux, Expr.zero, false);
	}

	// Production.List
        public static class List extends Comp.List {
            public List(int n) { super(n); }
	    public Production prod(int n) { return (Production) get(n); }
        }
}

