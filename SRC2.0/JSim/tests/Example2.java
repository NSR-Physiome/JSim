/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
/* EXAMPLE CODE ONLY

   example mainline for jssoap application
    load a project,  run the model,  query data in bursts
    mainline takes 4 arguments:
      name of project file 
      name of model with project
      name of variable for burst output
      number of data points in a burst (last burst may be shorter)

  This program:

    1) loads a project file;
    2) builds (compiles) a model;
    3) starts a model run thread;
    4) starts a query thread that polls for data
every POLLFREQ milliseconds.  If enough data is
present for a burst,  one or more bursts are written;
    5) waits for model run termination
    6) waits for query thread termination
    7) exits

--
   In this directory is an example project aslow.proj that
generates sine and cosine curve data points every 
100 milliseconds (using an MML source function which calls
Thread.sleep).  An example use of this program:

  jsrun.db JSim.soap.Example2 aslow.proj aslow u 10

   This will run the aslow model in aslow.proj and output
values of variable u in bursts of 10 data points.

EXAMPLE CODE ONLY */

package JSim.tests;

import java.io.*;	

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.project.*;

public class Example2 {

	// mainline
	public static final void main(String[] args)
	throws Exception {
	    new Example2(args);
	}

	// state
	public ASModel rt;
	public ASVar v;
	public int burstSize;
	public long POLLFREQ = 100; // milliseconds
	boolean running;

	// constructor
	public Example2(String[] args) throws Exception {

	    // parse command line
	    if (args.length != 4) throw new Xcept(
		"Expect 4 arguments to Example2");
	    JSReadable projFile = new JSReadable(args[0]);
	    String modelName = args[1];
	    String varName = args[2];
	    burstSize = Util.toInt(args[3]);

	    // load application server and project
	    PApplication appl = new PApplication();
	    Project proj = new Project("proj", appl);
	    proj.importXML(projFile);

	    // find and compile model
	    PModel pmodel = (PModel) proj.child(modelName);
	    PJob pjob = new PModelBuildJob(pmodel);
	    pjob.simpleRun();
	    rt = pmodel.rt();
	    
	    // find output variable
	    v = rt.getASVar(varName);
	    if (v.isInput() || v.isDomain() || v.ndim() != 1) 
	        throw new Xcept(v,
		    "Variable inappropriate for burst output");

	    // start model run thread
	    pjob = new PModelRunJob(pmodel);
	    pjob.start();
	    running = true;

	    // start query thread
	    QueryThread query = new QueryThread();
	    query.start();

	    // wait for model run to finish
	    pjob.join();
	    running = false;

	    // wait for final query and burst
	    query.join();
	}

	// query thread
	public class QueryThread extends Thread {
	    private int tstart;

	    // constructor
	    public QueryThread() {
		super("QueryThread");
	    }

	    // mainline: sleep, query and burst
	    public void run() {
		tstart = 0;
	    	try { 
		    while (true) {
		    	Thread.sleep(POLLFREQ); 
		    	Data data = rt.getData(0, v);
		    	if (data == null) {
			    System.err.println("No data yet");
			    continue;
		    	}
		    	burst(data);
		    	if (! running) break;
		    }
		    System.err.println("Exiting " + this);
		} catch (Exception e) { 
		    System.err.println(
			"Error in " + this + ": " + e);
		}
	    }

	    // write one or more bursts if required
	    public void burst(Data data) throws Xcept {
		// calculate burst window (tstart:tstop)
		double[] d = data.samples();
		if (d == null) return;
		int tmax = (data.subset == null) ? 
		    d.length : (data.subset.hix+1);
		if (! running) tmax = d.length;

		// complete bursts
		while (tmax >= tstart + burstSize) {
		    oneBurst(d, tstart, tstart+burstSize);
		    tstart += burstSize;
		}

		// final burst?
		if (!running && tstart<tmax)
		     oneBurst(d, tstart, tmax);
	    }

	    // write one data burst
	    public void oneBurst(double[] d, int tstart, int tstop) {
		System.out.print("oneBurst " + tstart + 
		    "-" + (tstop-1) + ":");
	    	for (int i=tstart; i<tstop; i++)
		    System.out.print(" " + ((float) d[i]));
	    	System.out.println("");
	    }

	}
}
