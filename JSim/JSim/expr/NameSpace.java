/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// component and unit name space

package JSim.expr;
import JSim.util.*;

public interface NameSpace {

	// get variable expression by name 
	public Expr compByName(String name) throws Xcept;

	// make deriv expression
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept;

	// get unit by name
	public Unit unitByName(String name) throws Xcept;

	// function call
	public Expr funcCall(String name, Expr.List elist) throws Xcept;
}


