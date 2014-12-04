/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file group

package JSim.xsim;

import JSim.util.*;

public class CFGroup extends CFItem {
	public String name;
	public String label;	// for menu 
	public int width, height;	// used?
	public boolean popup;
	public double pix_cfx, pix_cfy; // pixels per cf coord
	public int fontSize; 	// fontSize for scaling
	public double jsimReduce; // JSim reduction factor
	public PrettyFormat posFmt; // format for pos tags
	public CFGroupItem.List items;

	// constructor
	public CFGroup(CF c, String n) {
	    super(c);
	    name = safeName(n);
	    label = n;
	    width = height = 1000; // default cf group size
	    pix_cfx = pix_cfy = 0.5; // 500 pixels for 1000 cf	    
	    fontSize = 12;  // JSim default font size
	    jsimReduce = 1; // none for now
	    posFmt = new PrettyFormat(7);
	    items = new CFGroupItem.List();
	}

	// set attribute
	public void set(String key, String value) {
	    key = key.toLowerCase();
	    if (key.equals("width")) 
		width = toInt(value);
	    else if (key.equals("height")) 
		height = toInt(value);
	    else if (key.equals("popup")) 
		popup = true;
	    else if (key.equals("label"))  
		label = value;
	}

	// group coordinate conversion
	public double xscale(double cfx) {
	    double x = jsimReduce*cfx*pix_cfx/fontSize;
	    return Util.round(x, 0.1);
	}
	public double yscale(double cfy) {
	    double y = jsimReduce*cfy*pix_cfy/fontSize;
	    return Util.round(y, 0.1);
	}

	// write RTML page
	public void writeRTML() {
	    if (name.equals("Inputs")) return;
	    if (name.equals("Outputs")) return;
	    println("    <page name=\"" + name + "\">");
	    for (int i=0; i<items.size(); i++) {
		if (items.item(i) instanceof CFBoxPar)
		    continue;
		items.item(i).writeRTML();
	    }
	    println("    </page>");
	}

}
