/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file parameter box

package JSim.xsim;

import JSim.util.*;

public class CFBox extends CFGroupItem {
 	public CFGroupItem.List pars;

	// constructor
	public CFBox(CFGroup g) {
	    super(g);
	    pars = new CFGroupItem.List();
	}

	// set attribute
	protected void setItem(String key, String value) {
	    // nothing yet
	}

	// text width
	public int textWidth() {
	    int w = 0;
	    for (int i=0; i<pars.size(); i++) {
		int w1 = ((CFBoxPar) pars.item(i)).width();
		if (w1>w) w = w1;
	    }
	    return w;
	}

	// write RTML page
	public void writeRTML() {

	    println("    <table " + pos() +
		" widths=\"" + textWidth() + " 7 0\">");
	    for (int i=0; i<pars.size(); i++) 
		pars.item(i).writeRTML();
	    println("    </table>");
	}
}
