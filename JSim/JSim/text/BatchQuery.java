/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// query thread for run-time model queryLock testing

package JSim.text;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.aserver.*;

class BatchQuery extends Thread {
	// state
	private PModel pmodel; // model to query
	private long pause; // pause between queries
	private ASQuery expr; // expr to query
	private static long MINMESS = 200;
	private boolean term;

	// constructor
	public BatchQuery(PModel m, long p, String s) 
	throws Xcept {
	    super("query thread");
	    pmodel = m;
	    pause = p;
	    expr = pmodel.rt().parseQuery(s);
	    term = false;
	}

	// sleep for reqd time
	private void sleep() {
	    try {
		Thread.sleep(pause);
	    } catch (Exception e) { }
	}

	// set terminate
	public void terminate() { term = true; }

	// run 
	public void run() {
	    // wait until model starts running
//	    System.err.println("QueryTest thread waiting for model run to start ...");
	    while (!term && !pmodel.isRunning() && pmodel.rt().nstores()==0)
		sleep();
	    System.err.println("QueryTest thread now active ...");

	    // query while model is running
	    int ct = 0;
	    while (!term) {
		ct++;
		String msg = null;
	    	boolean show = false;
		try {
		    Data d = pmodel.rt().getData(0, expr);
		    if (d == null) {
			msg = "getData returned null";
			show = true;
		    } else {
			msg = "getData returned " + 
			    d.nsamples() + " points"; 
		    }
		} catch (Xcept e) {
		    msg = e.cleanMessage();
		    show = true;
		}
//		if (show) 
//		    System.err.println("QueryTest[" + 
//			ct + "] " + msg);
	   	sleep();
	    }
//	    System.err.println("QueryTest thread exiting ...");
	}
}


