/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// common planner interface for integrals, summation

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;

public interface IntegralIF {
	public Domain t();
	public Expr min();
	public Expr max();
	public Expr u();

	// evaluation in terms of private vars
	public Expr evalExpr();
}
