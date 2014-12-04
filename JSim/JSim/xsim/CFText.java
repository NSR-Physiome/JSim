/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file text label

package JSim.xsim;

import JSim.util.*;

public class CFText extends CFGroupItem {
	public String text;

	// constructor
	public CFText(CFGroup g, String t) {
	    super(g);
	    text = t;
	}

	// set attribute
	protected void setItem(String key, String value) {
	    // nothing yet
	}

	// write RTML
	public void writeRTML() {
	    println("\t<text " + pos() + 
		textAttr(text) + "/>");
	}
}
