/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// One label on a plot

package JSim.project;

import JSim.util.*;

public class PlotLabel extends PNamed {

	// controls
	public StringControl label;
	public RealControl labelX;
	public RealControl labelY;

	// constructor
	public PlotLabel(PNamed p, String n) throws Xcept {
	    super(p, n);
	    label = new StringControl(this, "label");
	    labelX = new RealControl(this, "min", 0);
	    labelY = new RealControl(this, "max", 1);
	}

	// query
	public String diagInfo() { return "PlotLabel " + name; }
	public String xmlLabel() { return "plotlabel"; }
}

