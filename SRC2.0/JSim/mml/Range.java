/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// range for integral or sum

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;

public class Range {
	public String t;
	public Expr min, max;
	public Range(String tt, Expr mn, Expr mx) {
	    t = tt;
	    min = mn;
	    max = mx;
 	}

	public String toString() { 
	    return t + "=" + min + " to " + max;
	}
}
