/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file independent variable

package JSim.xsim;

import JSim.util.*;

public class CFIvar extends CFItem {
	private String xsname;
	private String jsname;
	public int loc;
	public CFPar pcurr, pstart, pstop, pincr;

	// constructor
	public CFIvar(CF c, String n) {
	    super(c);
	    cf.ivar = this;
	    xsname = n;
	    pcurr = new CFPar(cf, CFPar.REAL, xsname);
	    pstart = new CFPar(cf, CFPar.REAL, xsname + "_start");
	    pstop = new CFPar(cf, CFPar.REAL, xsname + "_stop");
	    pincr = new CFPar(cf, CFPar.REAL, xsname + "_incr");
	    pcurr.skip = pstart.skip = pstop.skip = pincr.skip = true;
	}

	// set attribute
	public void set(String key, String value) {
	    key = key.toLowerCase();
	    if (key.equals("loc")) {
		loc = toInt(value);
		pstart.set(key, "" + (loc-2));
		pstop.set(key, "" + (loc-1));
		pincr.set(key, "" + (loc+1));
	    }
	}

	// query
	public String name() {
	    return pcurr.name();
	}

	// write MML
	public void writeMMLVar() {
	    println("\tivar " + name() + "; " + 
		name() + ".loc=" + loc + ";");
	    println("\t" + 
		eqn(pstart) + " " + 
		eqn(pstop) + " " +
		eqn(pincr));
	}

	// eqn for time control
	private String eqn(CFPar p) {
	    return p.name() + " = " + p.init + ";";
	}
}
