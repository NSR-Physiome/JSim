/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// PJob with graphic update

package JSim.gui;

import javax.swing.SwingUtilities;
import JSim.util.*;
import JSim.project.*;

abstract public class GJob {
	protected GNode gnode;
	protected PJob pjob;

	// constructor
	public GJob(GNode n)  {
	    gnode = n;
	}

	// GUI update prior to run
	abstract protected void gpre();

	// GUI update following run
	abstract protected void gpost();

	// query
	public GProject gproject() {  return gnode.gproject(); }
	public PJob pjob() { return pjob; }


}

