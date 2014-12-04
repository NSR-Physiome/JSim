/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// source of flow, possible recirculating

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class FlowSource extends Flow {

	// constructor
	public FlowSource(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    FlowComp fc;
	    if (args.size() == 1) {
		fc = (FlowComp) getParm(e,0,FlowComp.class);
	        attach(false, fc);
	    } else if (args.size() == 2) {
 		fc = (FlowComp) getParm(e,0,FlowComp.class);
                attach(true, fc);
 		fc = (FlowComp) getParm(e,1,FlowComp.class);
                attach(false, fc);
	    } else throw new Xcept(
		    "FlowSource " + n + " requires 1 or 2 arguments");
 	}

	// compatible unit
	public String compatibleUnit() { return "liter/second"; }

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    setVarUnit(flow, unit);
	}
}
