/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// nl2sol optimizer

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public class NL2sol extends Optimizer {

	// static class initialization 
	public static OptimAlg.Info algInfo() {
	    OptimAlg.Info algInfo = new OptimAlg.Info();
	    algInfo.name = "nl2sol";
	    algInfo.boundsNeeded = false;
	    algInfo.sensMatNeeded = false;
	    algInfo.parsNeeded = new String[] { };
	    algInfo.optimClassName = NL2sol.class.getName();
	    return algInfo;
	}

	// static class initialization - link to opt library
	static {
	    System.loadLibrary("opt");
	}

	// current state
	private OptimArgs args;
	private OptimCallbacks cbs;
	private OptimResults res;

	// run optimzer
	public void optimize(RTContext ctxt, OptimResults r, 
	OptimCallbacks c)
	throws Xcept {

	    // initialization
	    res = r;
	    args = res.args;
	    cbs = c;

	    // results and work area allocation
            int nr = 1;
	    int n = args.nx();
	    double[] x = new double[n];
	    for (int i=0; i<n; i++)
		x[i] = args.xstart[i];
	    int[] iv = new int[84+n];
            double[] v = new double[105+n*(nr+2*n+17)+3*nr];
	    int[] istop = new int[1];	    
	    int liv = 84+n;
            int lv = 105+n*(nr+2*n+17)+3*nr;
            int iout = -1;


	    // call native method (fortran)
	    try { 

		// call native method
		int threadInx = (ctxt == null) ? 0 : ctxt.threadInx;
		nl2sol(threadInx, ctxt, nr,n,x,iv,liv,v,lv,
		    new double[] { args.errTol },
                    args.maxCalls,
		    iout, istop);

	        // get simplex specific results
		res.status = istop[0];
	 	for (int i=0; i<n; i++) 
		    res.bestX[i] = x[i];
		res.finalStep = Double.NaN; // unavailable?
		switch (res.status) {
		case 0: 
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Normal termination";
		    break;
		case  1:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met Min RMS error stopping criterion";
		    break;
		case  2:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met # calls stopping criterion";
		    break;
		case  3:
		    res.status = OptimResults.ERROR;
		    if (Util.isBlank(res.termMsg))
		    	res.termMsg = "Optimization canceled by user";
		    break;
		default:
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Undocumented error code (" + istop[0] + ")";
		    break;
		}		    
		    

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
		throw new Xcept("NL2sol optimizer: " + res.termMsg);
	} 

	// native nl2sol method
	private native void nl2sol(
	    int threadInx,  // thread context index
	    RTContext ctxt, // evaluation context
            int nr, // # of residuals
	    int nx,   	// # pars to optimize
	    double[] x, // starting x vals, size nx, returns final vals
            int[] iv,
            int liv,
            double[] v,
            int lv,
            double[] fmin,
	    int maxfcn, // # fcn calls stop criterion 
            int iout,   // don't print
	    int[] istop  // termination status
	) throws Exception;

	// native abort 
	public native void abort(int threadInx);

	// Fortran request for function evaluation within simplx()
	public double fcn(RTContext ctxt, double[] x) {
	    try {
		return cbs.calcError(ctxt, x, res);
	    } catch (Exception e) {
		int threadInx = (ctxt==null) ? 0 : ctxt.threadInx;
		abort(threadInx);
		return Double.NaN;
	    }
	}

	// query
	public String diagInfo() { return "NL2sol Optimizer"; }
	public boolean allowMP() { return false; }
}


