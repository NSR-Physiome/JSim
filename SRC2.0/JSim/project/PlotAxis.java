/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// One Plot axis

package JSim.project;

import JSim.util.*;
import org.w3c.dom.Element;

public class PlotAxis extends PNamed {

	// constants
	public static final int X = 0;
	public static final int Y = 1;
	public static final int Z = 2;
	public static final String LABEL_DEFAULT = "axis label";

	// controls
	public BooleanControl log;
	public BooleanControl autoscale;
	public StringControl label;
	public BooleanControl showLabel;
	public RealControl min;
	public RealControl max;	

	// constructor
	public PlotAxis(PNamed p, String n) throws Xcept {
	    super(p, n);
	    log = new BooleanControl(this, "log", false,
		new String[] { "log", "linear" });
	    autoscale = new BooleanControl(this, "autoscale", true);
	    label = new StringControl(this, "label", LABEL_DEFAULT);
	    showLabel = new BooleanControl(this, "showLabel", true);
	    min = new RealControl(this, "min", 0);
	    max = new RealControl(this, "max", 1);
	}

	// import XML element into matching PNamed child
	public PNamed importXMLChild(Element c) {
	    String cname = c.getAttribute("name");

	    // ignore legacy fields (before 1.6.45)
	    if (cname.equals("labelPos")) return null;
	    if (cname.equals("ticDelta")) return null;
	    if (cname.equals("ticIntervals")) return null;
	    if (cname.equals("ticDigits")) return null;

	    return super.importXMLChild(c);
	}

	// query
	public String diagInfo() { return "PlotAxis " + name; }
	public String xmlLabel() { return "plotaxis"; }
}

