/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// single species PS-determined transporter across membrane

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class TransportPS extends Transport {
	protected Chem c;
	protected RealNVar PS;

	// constructor
	public TransportPS(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e,2);
	    mem = (Membrane) getParm(e,0,Membrane.class);
	    c = (Chem) getParm(e,1,Chem.class);
	    mem.attach(this);
	    PS = new RealNVar(this, "PS", sys.tlist());
	}

	// region concentration change
	public Expr concDelta(Region r, Chem ch) throws Xcept {
	    if (ch != c) return Expr.zero;
	    Region r1, r2;
	    if (r == mem.region1) {
		r1 = mem.region1;
		r2 = mem.region2;
	    } else if (r == mem.region2) {
		r1 = mem.region2;
		r2 = mem.region1;
	    } else 
		throw new Xcept(this, "Cannot get regionDelta of " +
		    r.name());
	    return PS.div(r.vol).mult(r2.conc(c).sub(r1.conc(c)));
	}

	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    Unit psunit = mem.region1.unit().div(timeUnit());
	    setVarUnit(PS, psunit);
	}
}
