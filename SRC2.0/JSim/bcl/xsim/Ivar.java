/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XSim Independent Variable (ivar)

package JSim.bcl.xsim;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.io.PrintStream;

public class Ivar extends Domain {
	private IntNVar vloc;

	// constructor
	public Ivar(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    if (! (p instanceof XSSys)) throw new Xcept(this,
		"parent must be XSim");
	    XSSys sys = (XSSys) p;
	    if (sys.t != null) 
		throw new Xcept(this, "duplicate ivar");
	    sys.t = this;
	    XSVar.common(this);
	}

}
