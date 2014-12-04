/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// scalar choice variable

package JSim.jruntime;

import JSim.util.*;

public class RTChoiceNVar extends RTIntNVar {
	private String[] labels;
	private String[] labelVals;

	// constructor
	public RTChoiceNVar(RTModel m, String n, String u, int ph,
	RTRealDomain[] d, String[] labs, String[] labVals) throws Xcept {
	    super(m, n, u, ph, d);
	    labels = labs;
	    labelVals = labVals;
	}

	// query
	public String[] labels() { return labels; }
	public String[] labelValues() { return labelVals; }
}

