/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// single species flux-based transporter across membrane

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class TransportFlux extends Transport {
	protected Chem c;
	protected RealNVar flux;

	// constructor
	public TransportFlux(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e,2);
	    mem = (Membrane) getParm(e,0,Membrane.class);
	    c = (Chem) getParm(e,1,Chem.class);
	    mem.attach(this);
	    flux = new RealNVar(this, "flux", sys.tlist());
	}

	// region concentration change
	public Expr concDelta(Region r, Chem ch) throws Xcept {
	    if (ch != c) return Expr.zero;
	    Expr pflux = flux.div(r.vol);
	    if (r == mem.region1)
		return Expr.negone.mult(pflux);
	    else if (r == mem.region2)
		return pflux;
	    else
		return Expr.zero;
	}

	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    setVarUnit(flux, c.fluxUnit());
	}
}
