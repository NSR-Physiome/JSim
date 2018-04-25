/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// optional set of notes 

package JSim.project;

import JSim.util.*;

public class PSemSim extends PNamed {

	// controls
	public SemSimControl text;

	// constructor
	public PSemSim(PNamed p, String n) throws Xcept {
	    super(p, n);
	    addDescCntl();
	    text = new SemSimControl(this, "semsim");
	}

	// query
	public String diagInfo() { return "PSemSim " + name; }
	public String xmlLabel() { return "semsim"; }
}

