/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// general transporter across membrane

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

abstract public class Transport extends MFComp {
	Membrane mem;

	// constructor
	public Transport(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
 	}

	// region flux term
	abstract public Expr concDelta(Region r, Chem ch) throws Xcept;

	// Transport.List
        public static class List extends Comp.List {
            public List(int n) { super(n); }
        }
}
