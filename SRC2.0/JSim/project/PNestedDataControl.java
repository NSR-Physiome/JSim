/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// expr to plot from PDataSet or PModel

package JSim.project;

import java.util.*;

import JSim.util.*;
import JSim.data.*;

public class PNestedDataControl extends DataControl {

	// constructor
	public PNestedDataControl(PNamed p, String n, PNamed b) 
	throws Xcept {
	    super(p, n, b);
	}

	// pick dimension from plot
	protected int pickName(int ndim) {
	    return (ndim == 0) ? NO_NAME : SHORT_NAME;
	}
}

