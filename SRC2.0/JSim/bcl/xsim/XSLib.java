/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XSim dynamic model library

package JSim.bcl.xsim;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class XSLib extends Comp {
	private XSSys sys;
	private String libName;

	// constructor
	public XSLib(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    try {
	    	sys = (XSSys) p;
	    } catch (ClassCastException e1) {
		throw new Xcept(p, 
		name + " parent must be XSim.XSSys");
	    }
	    if (e.size() != 1 || !(e.expr(0) instanceof StringConst))
		throw new Xcept(this, 
		    "single String argument required");
	    libName = ((StringConst) e.expr(0)).constStringVal();
	    if (sys.lib != null) throw new Xcept(this, "duplicate Library");
	    sys.lib = this;
	    if (Util.isBlank(libName)) throw new Xcept(sys,
		"blank library name not allowed"); 
	    if (! Util.onlyLettersAndDigits(libName))
		throw new Xcept(sys,
		    "library name may contain only letters and digits");
 	}

	// query
	public String libName() { return libName; }
}
