/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Irreversible Simple Michaelis-Menten reaction

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class ReactionUUI extends ReactionFlux {
	RealNVar Vm, Km;

	// constructor
	public ReactionUUI(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    if (eqn.rchem.size() != 1 ||
		eqn.lchem.size() != 1) throw new Xcept(this,
		"ReactionUUI requires single substate & product");
	    Vm = new RealNVar(this, "Vm",  sys.tlist());
	    Km = new RealNVar(this, "Km",  sys.tlist());
	    Expr S = region.conc(eqn.lchem.chem(0));
	    Expr fx = Vm.mult(S).div(Km.add(S));
	    setVar(flux, fx, true);
	}

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    Chem c = eqn.lchem.chem(0);
	    Unit vmunit = c.unit().div(timeUnit());
	    setVarUnit(Vm, vmunit);
	    setVarUnit(Km, c.unit());
	}
}
