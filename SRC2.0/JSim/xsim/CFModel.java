/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file model declaration

package JSim.xsim;

import JSim.util.*;

public class CFModel extends CFItem {

	// constructor
	public CFModel(CF c) {
	    super(c);
	}

	// set attribute
	public void set(String key, String value) {
	    key = key.toLowerCase();
	}
}
