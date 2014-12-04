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

abstract public class CFGroupItem extends CFItem {
	public CFGroup group;
	public int x, y, w, h;
	public String font;

	// constructor
	public CFGroupItem(CFGroup g) {
	    super(g.cf);
	    group = g;
	    group.items.add(this);
	}

	// set dim
	public void setDim(String[] d) {
	    try {
	    	x = Util.toInt(d[0]);
	    	y = Util.toInt(d[1]);
	    	w = Util.toInt(d[2]);
	    	h = Util.toInt(d[3]);
	    } catch (Xcept e) {
		error("toInt() error in setDim for group item " + this);
	    }
	}

	// set attribute
	public void set(String key, String value) {
	    key = key.toLowerCase();
	    if (key.equals("font")) 
		font = value;
	    setItem(key, value);
	}
	abstract protected void setItem(String key, String value);

	// position / size strings
	public String pos() {
	    String sx = group.posFmt.format(group.xscale(x));
	    String sy = group.posFmt.format(group.yscale(y));
	    return"pos=\"" + sx + " " + sy + "\""; 
	}
	public String size() {
	    String sw = group.posFmt.format(group.xscale(w));
	    String sh = group.posFmt.format(group.yscale(h));
	    return"size=\"" + sw + " " + sh + "\""; 
	}

	// write RTML
	public void writeRTML() {
	    println("\tclass name=\"" + 
		this.getClass().getName() + "\"/>");
	}
}

