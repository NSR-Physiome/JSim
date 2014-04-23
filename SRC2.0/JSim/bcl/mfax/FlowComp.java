/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// component that connects to Flow

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

abstract public class FlowComp extends MFComp {
	protected Flow.List inflows;	// attached inflows
	protected Flow.List outflows;	// attached outflows

	// constructor
	public FlowComp(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    inflows = new Flow.List(1);
	    outflows = new Flow.List(1);
 	}

	// attach Flow 
	protected void attach(boolean isin, Flow f) throws Xcept {
	    if (isin) inflows.add(f); else outflows.add(f);
	}
}
