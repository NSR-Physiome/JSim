/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Chemical species

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Chem extends MFComp {
	Unit fluxUnit; // unit * liter / time (perhaps simplified)

	// constructor
	public Chem(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e, 0);
	    if (!Character.isUpperCase(n.charAt(0))) 
		throw new Xcept(this,
		   "Chem component names must start with upper-case letter");

	    // add to system and other components
	    sys.chem.add(this);
	    for (int i=0; i<sys.child.size(); i++) {
		Comp comp = sys.child.comp(i);
		if (comp.isVar()) continue;
		((MFComp) comp).addChem(this);
	    }
 	}

	// compatible unit
	public String compatibleUnit() { return "mole/liter"; }

	// moles compatible unit
	public Unit fluxUnit() throws Xcept {
	    if (fluxUnit == null) {
		fluxUnit = unit.mult(sys.liter());
		fluxUnit = sys.simplify(fluxUnit);
	    	fluxUnit = fluxUnit.div(timeUnit());
	    }
	    return fluxUnit;
	}

        // Chem.List
        public static class List extends Comp.List {
            public List(int n) { super(n); }
	    public Chem chem(int i) { return (Chem) get(i); }
        }
}
