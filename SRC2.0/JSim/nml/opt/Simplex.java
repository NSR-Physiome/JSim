/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Simplex optimizer 

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public class Simplex extends Optimizer {

	// static class initialization 
	static {
	    System.loadLibrary("opt"); // native library
	}
	public static OptimAlg.Info algInfo() {
	    OptimAlg.Info algInfo = new OptimAlg.Info();
	    algInfo.name = "simplex";
	    algInfo.boundsNeeded = true;
	    algInfo.sensMatNeeded = false;
	    algInfo.parsNeeded = new String[] { "stepTol", "xstep" };
	    algInfo.optimClassName = Simplex.class.getName();
	    return algInfo;
	}

	// current state
	private OptimArgs args;
	private OptimCallbacks cbs;
	private OptimResults res;

	// run optimzer
	public void optimize(RTContext ctxt, OptimResults r, 
	OptimCallbacks cbs) throws Xcept {

	    // initialization
	    res = r;
	    args = res.args;
	    this.cbs = cbs;
	    if (Util.hasNaNs(args.xmin) ||
		Util.hasNaNs(args.xmax)) throw new Xcept(this,
		"xmin & xmax required");

	    // xstart defaults to min+max/2
	    for (int i=0; i<args.nx(); i++) 
		if (Double.isNaN(args.xstart[i]))
		    args.xstart[i] = (args.xmax[i]+args.xmin[i])/2;

	    // results and work area allocation
	    int n = args.nx();
	    int[] nfout = new int[2];
	    double[] tlout = new double[2];
	    int[] istat = new int[1];
	    double[] p = new double[n*(n+1)];
	    double[] y = new double[n+1];
	    int[] fnums = new int[n+1];
	    double[] pbar = new double[n];
	    double[] pstar = new double[n];
	    double[] pdstar = new double[n];

	    // call native method (fortran)
	    try { 
	    	int threadInx = (ctxt == null) ? 0 : ctxt.threadInx;
		simplx(threadInx, ctxt, 
		n, // int != jint, must calloc
		args.xstart, args.xmin, args.xmax, args.xistep, 
		new double[] { args.stepTol }, // saves calloc
		args.maxCalls, // int != jint, must calloc
		new double[] { args.errTol }, // saves calloc
	    	res.bestX, 
		nfout, tlout, istat, 
	    	p, y, fnums, pbar, 
	    	pstar, pdstar);

	        // get simplex specific results
		res.status = istat[0];
		res.finalStep = tlout[1];

		switch (istat[0]) {
		case 4: 
		    res.status = OptimResults.ERROR;
		    if (Util.isBlank(res.termMsg)) 
		        res.termMsg = "Optimiation canceled by user";
		    break;
		case 3:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met # calls stopping criterion";
		    break;
		case 2:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "All vertices have same func value";
		    break;
		case 1:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met step size stopping criterion";
		    break;
		case 0:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met mean sqr error stopping criterion";
		    break;
		case -1:
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Nonsensical parameters supplied";
		    break;
		case -2:
		    res.status = OptimResults.CANCEL;
		    res.termMsg = "Error midway through optimization";
		    break;
		case -3:
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Error during first func evaluation";
		    break;
		case -4:
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Canceled during first func evaluation";
		    break;
		default:
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Undocumented error code (" + istat[0] + ")";
		    break;
		}		    

	    // pretty up any errors
	    } catch (Exception e) { 
		if (e instanceof Xcept) {
		    Xcept xe = (Xcept) e;
		    res.termMsg = xe.cleanMessage();
		    throw xe;
		} else {
		    res.termMsg = e.getMessage();
		    throw new Xcept(this, res.termMsg);
		}
	    }

	    if (res.status == OptimResults.ERROR) 
		throw new Xcept("Simplex optimizer: " + res.termMsg);
	} 

	// native Simplex method 
	private native void simplx(
	    // inputs
	    int threadInx, // thread context index
	    RTContext ctxt, // fcn evaluation context
	    int nx,   	// # pars to optimize
	    double[] xstart, // starting x vals, size nx
	    double[] xmin, // min x vals, size nx
	    double[] xmax, // max x vals, size nx
	    double[] xistep, // initial absolute step sizes, size nx
	    double[] steptl, // [0]=relative step tolerance
	    int maxfn, // # fcn calls tolerance (rough)
	    double[] fcntl, // [0]=min fcn value tolerance

	    // outputs
	    double[] xout, // optimized x values 
	    int[] nfout, // [0]=best fcn call; [1]=# fcn calls
	    double[] tlout, // [0]=min fcn value; [1]=last step value
	    int[] istat, // [0]=return status

	    // scratch work areas
	    double[] p, // size (nx+1)*nx
	    double[] y, // size nx+1
	    int[] fnums, // size nx+1
	    double[] pbar, // size nx
	    double[] pstar, // size nx
	    double[] pdstar // size nx 
	) throws Exception;

	// native abort simplex 
	public native void abort(int threadInx);

	// Fortran request for function evaluation within simplx()
	public double fcn(RTContext ctxt, double[] x) {
	    try {
		return cbs.calcError(ctxt, x, res);
	    } catch (Exception e) {
	    	int threadInx = (ctxt == null) ? 0 : ctxt.threadInx;
		abort(threadInx);
		return Double.NaN;
	    }
	}

	// query
	public String diagInfo() { return "Simplex Optimizer"; }  
	public boolean allowMP() { return false; }
}


