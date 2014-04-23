/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Time

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Time extends Domain {
	MFSys sys;

	// constructor
	public Time(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    try {
	    	sys = (MFSys) p;
	    } catch (ClassCastException e1) {
		throw new Xcept(p, 
		name + " parent must be MFAX.MFSys");
	    }
	    if (sys.t != null) throw new Xcept(this, "duplicate Time");
	    sys.t = this;
 	}
}
