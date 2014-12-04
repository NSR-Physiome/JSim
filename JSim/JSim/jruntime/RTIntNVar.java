/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// n-dimensional real variable

package JSim.jruntime;

import JSim.util.*;

public class RTIntNVar extends RTRealNVar {

	// constructor
	public RTIntNVar(RTModel m, String n, String u, int ph,
	RTRealDomain[] d) throws Xcept {
	    super(m, n, u, ph, d);
	}
}

