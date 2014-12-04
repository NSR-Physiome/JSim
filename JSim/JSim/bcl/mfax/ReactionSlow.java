/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// slow Chemical reaction

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.util.ArrayList;

abstract public class ReactionSlow extends Reaction {

	// constructor
	public ReactionSlow(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	}

	// query
	public boolean isFast() { return false; }

	// region concentration deriv wrt time
	abstract public Expr concDelta(Chem ch) throws Xcept;
}
