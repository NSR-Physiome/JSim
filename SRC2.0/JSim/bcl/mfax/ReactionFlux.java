/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// arbitrary Flux-based reaction

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.util.ArrayList;

public class ReactionFlux extends ReactionSlow {
	RealNVar flux;	

	// constructor
	public ReactionFlux(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    flux = new RealNVar(this, "flux", sys.tlist());
	}

	// region concentration change
	public Expr concDelta(Chem ch) throws Xcept {
	    double tot = eqn.factor(ch);
	    if (tot == 0) return Expr.zero;
	    return flux.mult(Expr.cons(tot)).div(region.vol);
	}

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    Chem c = eqn.lchem.chem(0);
	    setVarUnit(flux, c.fluxUnit());
	}
}
