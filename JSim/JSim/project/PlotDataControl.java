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

public class PlotDataControl extends DataControl {
	private boolean phase;  // is phase expression

	// constructor
	public PlotDataControl(PNamed p, String n, PNamed b, boolean ph) 
	throws Xcept {
	    super(p, n, b);
	    phase = ph;
	}

	// pick dimension from plot
	protected int pickName(int ndim) {
	    return pickName(pickDim(), ndim);
	}
	private int pickDim() {  
	    if (!phase) {
	    	Plot plot = (Plot) ancestor(Plot.class);
	    	if (plot.style.val() != Plot.XY_PLOT)
		    return 2;
	    }
	    return 1;
	}
}

