/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// optimizer graph controls

package JSim.project;

import JSim.util.*;
import org.w3c.dom.Element;

public class PModelOptimGraph extends PNamed {

	static public final String[] NAMES = {
	    "pars", "normPars", "rmsError", "dataToMatch",
	    "unwgtResid", "wgtResid", "pointWgts"
	};

	static public final int PARAMETERS = 0; 
	static public final int NORMALIZED_PARS = 1; 
	static public final int RMS_ERROR = 2; 
	static public final int DATA_TO_MATCH = 3;
	static public final int UNWEIGHTED_RESIDUALS = 4;
	static public final int WEIGHTED_RESIDUALS = 5;
	static public final int POINT_WEIGHTS = 6;
	static public final int NGRAPHS = NAMES.length;

	// controls
	public BooleanControl log;

	// constructor
	public PModelOptimGraph(PModelOptim p, String n) throws Xcept {
	    super(p, n);
	    log = new BooleanControl(this, "log", false,
		new String[] { "log", "linear" });
	}

	// query
	public String diagInfo() { 
	    return "Optimizer Graph " + name + " for model " + pmodel(); 
	}
	public String xmlLabel() { return "optgraph"; }
}

