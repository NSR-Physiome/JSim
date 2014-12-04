/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Test Job

package JSim.tests; import JSim.project.*;

import JSim.util.*;

public class TestJob extends PJob {

	// state
	private PModel pmodel;

	// constructor
	public TestJob(PModel p) {
	    super(p, 666);
	    pmodel = p;
	    setName("Test job: model " + pmodel.name());
	}

	// run
	public void stoppable() throws Xcept {
	    int ct = 10;
	    for (int i=0; i<ct; i++) {
		System.err.println(getName() + 
		    " " + i + "/" + ct + " complete");
		try { sleep(500); } catch (Exception e) { }
	    }
	} 

	// postrun
	public void postRun() throws Xcept {
	    if (stat() == NORMAL)		   
	        System.err.println(getName() + " finished");
	    else
		System.err.println(getName() + " mop-up");
	}

	// query
	public String phaseDesc() { return "test phase"; }
	public double phaseFrac() { return .5; }
}
