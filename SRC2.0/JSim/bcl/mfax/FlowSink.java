/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// flow sink

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class FlowSink extends Flow {

	// constructor
	public FlowSink(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e,1);
	    FlowComp fc = (FlowComp) getParm(e,0,FlowComp.class);
	    attach(true, fc);
	}
}
