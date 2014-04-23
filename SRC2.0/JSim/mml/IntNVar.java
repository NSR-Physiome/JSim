/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// N-dimensional dependent integer variable

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

public class IntNVar extends NVar {
	
	// constructor
	public IntNVar(Comp p, String n, Expr.List d) 
	throws Xcept {
	    super(p, n, d);
	}

	// Var query methods
	public boolean isInt() { return true; }
}

