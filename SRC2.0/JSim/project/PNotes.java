/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// optional set of notes 

package JSim.project;

import JSim.util.*;

public class PNotes extends PNamed {

	// controls
	public TextControl text;

	// constructor
	public PNotes(PNamed p, String n) throws Xcept {
	    super(p, n);
	    addDescCntl();
	    text = new TextControl(this, "text");
	}

	// query
	public String diagInfo() { return "PNotes " + name; }
	public String xmlLabel() { return "notes"; }
}

