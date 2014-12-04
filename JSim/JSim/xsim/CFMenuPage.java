/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// page entry in XSim CF menu

package JSim.xsim;

import JSim.util.*;

public class CFMenuPage extends CFItem {
	public String name;
	public String label;

	// constructor
	public CFMenuPage(CF c, String n) {
	    super(c);
	    name = null;
	    label = n;
	}

	// post-processing: link label to appropriate page
	public void post() {
	    String slabel = safeName(label);
	    for (int i=0; i<cf.items.size(); i++) {
		CFItem item = cf.items.item(i);
		if (! (item instanceof CFGroup)) continue;
		CFGroup g = (CFGroup) item;
		if (g.name.equalsIgnoreCase(label) ||
		    g.name.equalsIgnoreCase(slabel) ||
		    g.label.equalsIgnoreCase(label) ||
		    g.label.equalsIgnoreCase(slabel)) {
		    name = g.name;
		    label = g.label;
		    return;
		}
	    }
	    warn("Menu page " + label + " not found");
	}		    

	// has content
	public boolean hasContent() {
	    return name != null;
	}

	// write RTML page
	public void writeRTML(String indent) {
	    if (! hasContent()) return;
	    String slab = "";
	    if (label != null && !name.equals(label))
		slab = textAttr(label);
	    println(indent + "<page name=\"" + name + "\"" + 
		slab + "/>");
	}

}
