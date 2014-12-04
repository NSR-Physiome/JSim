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

public class CFFeed extends CFItem {
	public String name;
	public int loc;
	public CFPar pselect, pfselect;

	// constructor
	public CFFeed(CF c, String n) {
	    super(c);
	    name = n;
	    pselect = new CFPar(cf, CFPar.CHOICE, name + "_select");
	    pselect.skip = true;
	    pfselect = new CFPar(cf, CFPar.CHOICE, name + "_fselect");
	    pfselect.skip = true;
	}

	// set attribute
	public void set(String key, String value) {
	    key = key.toLowerCase();
	    if (key.equals("loc")) {
		loc = toInt(value);
	    }
	}

	// write MML
	public void writeMMLVar() {
	    println("\trealInput " + name + "(" +
		cf.ivar.name() + ") = 0; " + 
		name + ".loc=" +  loc + ";");
	}
}
