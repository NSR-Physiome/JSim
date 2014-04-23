/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// abort exception

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 

public class AbortXcept extends Xcept {
	public AbortXcept(String msg) { super(msg); }
}
