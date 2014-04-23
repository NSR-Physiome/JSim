/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// inject Chem into Flow

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Inject extends MFComp {
	Flow flow;
	Chem chem;
	RealNVar flux;

	// constructor
	public Inject(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
 	    checkNParm(e,2);
	    flow = (Flow) getParm(e,0,Flow.class);
	    chem = (Chem) getParm(e,1,Chem.class);
	    flow.attach(this);
	    flux = new RealNVar(this, "flux", sys.tlist());
	}

	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    setVarUnit(flux, chem.fluxUnit());
	}

        // Inject.List
        public static class List extends Comp.List {
          public List(int n) {
            super(n);
          }
        }
}
