/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Reversible Mass Action Kinetics

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.util.ArrayList;

public class ReactionMassr extends ReactionSlow {
	RealNVar kf, kb;	

	// constructor
	public ReactionMassr(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    kf = new RealNVar(this, "kf", sys.tlist());
	    kb = new RealNVar(this, "kb", sys.tlist());
	}

	// region concentration change
	public Expr concDelta(Chem ch) throws Xcept {

	    // net gain ?
	    double tot = eqn.factor(ch);
	    if (tot == 0) return Expr.zero;

	    // fwd/bwd terms
	    Expr lx = kf;
	    for (int i=0; i<eqn.lchem.size(); i++) {
		Chem c = eqn.lchem.chem(i);
		lx = lx.mult(region.conc(c).pow(eqn.lfactor(c)));
	    }
	    Expr rx = kb;
	    for (int i=0; i<eqn.rchem.size(); i++) {
		Chem c = eqn.rchem.chem(i);
		rx = rx.mult(region.conc(c).pow(eqn.rfactor(c)));
	    }

	    // combine
	    return (tot<0) ? 
		rx.sub(lx).mult(Expr.cons(-tot)) : 
		lx.sub(rx).mult(Expr.cons(tot));
	}

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    Chem c = eqn.rchem.chem(0);
	    double n = eqn.rtot()-1;
	    Unit kbunit = Unit.scalar(); 
	    if (n!=0) kbunit = kbunit.div(c.unit().power(n));
	    kbunit = kbunit.div(timeUnit());
	    setVarUnit(kb, kbunit);
	    n = eqn.ltot()-1;
	    c = eqn.lchem.chem(0);
	    Unit kfunit = Unit.scalar(); 
	    if (n!=0) kfunit = kfunit.div(c.unit().power(n));
	    kfunit = kfunit.div(timeUnit());
	    setVarUnit(kf, kfunit);
	}
}
