/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test 2 computational threads

package JSim.tests; import JSim.util.*;

public class MPTest1 extends Thread {
	private int npts;  // # points
	
	// constructor
	public MPTest1(int n) {
	    npts = n;
	}
	
	// run with exception
	public void run() {
	    double t = 0;
	    double sin;
	    for (int i=0; i<npts; i++) {
	    	sin = Math.sin(t);
		t += .01;
	    }
	}
	
	// test dispatcher
	public static void main(String[] args) throws Exception {
	    if (args.length != 2) throw new Xcept(
	    	"Usage: #pts #threads");
	    int npts = Util.toInt(args[0]);
	    int nthreads = Util.toInt(args[1]);

	    // # processors
	    int nproc = Runtime.getRuntime().availableProcessors();
	    System.err.println("#processors=" + nproc);
	    long tstart = System.currentTimeMillis();	    

	    // start threads
	    Thread[] threads = new Thread[nthreads];
	    for (int i=0; i<nthreads; i++) {
	        threads[i] = new MPTest1(npts/nthreads);
		threads[i].start();
		System.err.println("starting thread #" + i);
	    }
	    
	    // wait till done
	    for (int i=0; i<nthreads; i++) {
		threads[i].join();
		System.err.println("finished thread #" + i);
	    }

	    // timing characteristics
	    long telapsed = System.currentTimeMillis() - tstart;
	    System.err.println("Elapsed time (msec)=" + telapsed);
	    System.err.println("SIN/msec = " + npts/telapsed);
	    System.err.println("SIN/msec/thread = " + npts/(telapsed*nthreads));
//	    System.err.println("SIN/msec/proc = " + npts/(telapsed*nproc));
	}
}
	    
