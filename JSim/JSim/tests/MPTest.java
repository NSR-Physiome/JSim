/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test MPDispatch with large trig calculation

package JSim.tests; import JSim.util.*;

public class MPTest implements MPDispatch.Job {
	private int npts;  // # points
	private double amp; // amplitude
	private double delta; // delta value
	private boolean canceled; // run cancelled?
	
	// constructor
	public MPTest(int n, double a, double d) {
	    npts = n;
	    amp = a;
	    delta = d;
	}
	
	// run with exception
	public void jobRunX(int workerInx) throws Xcept {
	    double t = 0;
	    double sin;
	    for (int i=0; i<npts; i++) {
	        if (canceled) return;
	    	sin = amp*Math.sin(t);
		t += delta;
	    }
	}
	
	// cancel run
	public void jobCancel() { canceled = true; }

	// skip  run
	public void jobSkip() { canceled = true; }

	// query
	public String jobName() { return "amp=" + amp; }

	// test dispatcher
	public static void main(String[] args) throws Xcept {
	    if (args.length != 3) throw new Xcept(
	    	"Usage: #points #jobs #threads");
	    int npts = Util.toInt(args[0]);
	    int njobs = Util.toInt(args[1]);
	    int nthreads = Util.toInt(args[2]);

            // # processors
            int nproc = Runtime.getRuntime().availableProcessors();
            System.err.println("#processors=" + nproc);
	    long tstart = System.currentTimeMillis();	    
	    
	    // create Job array
	    MPDispatch.Job[] jobs = new MPDispatch.Job[njobs];
	    for (int i=0; i<njobs; i++) 
	    	jobs[i] = new MPTest(npts/njobs, (i+1), .01);
		
	    // dispatch jobs
	    MPDispatch dispatch = new MPDispatch("Test1", jobs, 
	        new MPDispatch.SimpleMonitor());
	    dispatch.run(nthreads);

	    // timing characteristics
	    long telapsed = System.currentTimeMillis() - tstart;
	    System.err.println("Elapsed time (msec)=" + telapsed);
	    System.err.println("PTS/msec = " + npts/telapsed);
	    System.err.println("PTS/msec/thread = " + npts/(telapsed*nthreads));
	    if (nproc > 1)
		System.err.println("PTS/msec/proc = " + npts/(telapsed*nproc));

	}
}
	    
