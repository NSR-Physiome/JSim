/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file button

package JSim.xsim;

import JSim.util.*;

public class CFButton extends CFGroupItem {
	public static final int PAR = 1;
	public static final int GROUP = 2;
	public static final int FEED = 3;

	public int type;  
	public String name; 
	public String label;

	// constructor
	public CFButton(CFGroup g, int t, 
	String n) {
	    super(g);
	    type = t;
	    name = safeName(n);
	    label = n;
	}

	// set attribute
	protected void setItem(String key, String value) {
	    if (key.equals("label")) 
		label = value;
	}

	// Y-centered position
	private String centPos() {
	    String sx = group.posFmt.format(group.xscale(x));
	    double y1 = y + h/2;
	    double buttonHeight = 2;  // in text units
	    double y2 = group.yscale(y1)-buttonHeight/2;	    
	    String sy = group.posFmt.format(y2);
	    return"pos=\"" + sx + " " + sy + "\""; 
	}

	// write RTML
	public void writeRTML() {
	    if (type == FEED) return;
	    String tag = (type == PAR) ? 
		"varButton" : "pageButton";
	    String slabel = "";
	    if (label != null && ! label.equals(name)) 
		slabel = textAttr(label);
	    println("\t<" + tag + " " + centPos() + 
		" name=\"" + name + "\"" + slabel + "/>");
	}

}
