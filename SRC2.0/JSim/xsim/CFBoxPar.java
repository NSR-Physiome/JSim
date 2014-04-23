/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file parameter in CFBox

package JSim.xsim;

import JSim.util.*;

public class CFBoxPar extends CFGroupItem {
	public CFBox box;
	public String name; 
	public String label;

	// constructor
	public CFBoxPar(CFBox b, String n) {
	    super(b.group);
	    box = b;
	    name = safeName(n);
	    label = n;
	    box.pars.add(this);
	}

	// set attribute
	protected void setItem(String key, String value) {
	    if (key.equals("label")) 
		label = value;
	}

	// width
	public int width() {
	    String s = (label == null) ? name : label;
	    return s.length();
	}

	// write RTML
	public void writeRTML() {
	    String slab = "";
	    if (label != null && !name.equals(label))
		slab = textAttr(label);
	    println("\t<var " + 
		" name=\"" + name + "\"" + slab + "/>");
	}
}
