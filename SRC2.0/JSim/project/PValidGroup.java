/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// validated group with enable/disable control

package JSim.project;

import JSim.util.*;

abstract public class PValidGroup extends PNamed {

	// constructor
	public PValidGroup(PNamed p, String n) throws Xcept {
	    super(p, n);
	}

	// group enabled?
	public final boolean enabled() { 
	    return enableControl().val();
	}

	// is group valid?
	public final boolean valid() { 
	    return validMsg() == null; 
	}

	// enable/disable control
	abstract public BooleanControl enableControl();

	// is line blank?
	abstract public boolean isBlank();

	// validation message
	abstract  public String validMsg();
}

