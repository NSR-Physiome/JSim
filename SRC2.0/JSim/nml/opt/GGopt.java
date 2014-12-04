/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ggopt optimizer

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public class GGopt extends Optimizer {

	// static class initialization 
	static {
	    System.loadLibrary("opt"); // native library load
	}
	public static OptimAlg.Info algInfo() {
	    OptimAlg.Info algInfo = new OptimAlg.Info();
	    algInfo.name = "ggopt";
	    algInfo.boundsNeeded = false;
	    algInfo.sensMatNeeded = false;
	    algInfo.parsNeeded = new String[] { "stepTol", "maxIters", "gradTol", "eps" };
	    algInfo.optimClassName = GGopt.class.getName();
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

	    // results and work area allocation
	    int n = args.nx();
	    double[] x = new double[n];
	    for (int i=0; i<n; i++)
		x[i] = args.xstart[i];
	    double[] wk = new double[(1+n+n*n)*(1+n+n*(n+1)/2)];
	    int[] istop = new int[1];	    
	    int mxdim = n;
	    int mxcol = 1+mxdim+mxdim*(mxdim+1)/2;
	    int mxrow=1+mxdim+mxdim*mxdim;

	    double[] x0 = new double[mxdim];
	    double[] h = new double[mxdim];
	    double[] s = new double[mxdim];
	    double[] theta = new double[mxcol];
	    double[] wk1 = new double[mxrow*(mxdim+1)]; 
	    // GR Nov 05: exact wk1 dimension would be Math.min(mxdim,2)
	    double[] wk3 = new double[mxrow];
	    double[] wk4 = new double[mxcol];

	    // call native method (fortran)
	    try { 

		// call native method
		int threadInx = (ctxt == null) ? 0 : ctxt.threadInx;
		ggopt(threadInx, ctxt, n,x,args.maxCalls,args.maxIters,
		    new double[] { args.gradTol },
		    new double[] { args.stepTol },
		    new double[] { args.errTol },
		    new double[] { args.eps },
		    wk,istop,
		    mxdim, mxcol, mxrow, 
		    x0, h, s, theta,
		    wk1, wk3, wk4);

	        // get simplex specific results
		res.status = istop[0];

// return value fault according to GR 28 Aug 2007
//   see JSIM/BUGS/GR_OPT
//   till fixed,  used JSim calculate "best"
//	 	for (int i=0; i<n; i++) 
//		    res.bestX[i] = x[i];

		res.finalStep = Double.NaN; // unavailable?
		switch (res.status) {
		case 0: 
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Abnormal termination";
		    break;
		case 1:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met gradient stopping criterion";
		    break;
		case 2:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met relative step stopping criterion";
		    break;
		case 3:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met # iterations stopping criterion";
		    break;
		case 4:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Cannot improve values any further";
		    break;
		case 5:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met mean sqr error stopping criterion";
		    break;
		case 6:
		    res.status = OptimResults.NORMAL;
		    res.termMsg = "Met # calls stopping criterion";
		    break;
		case 7:
		    res.status = OptimResults.ERROR;
		    res.termMsg = "Error during model evaluation";
		    break;
		case 8:
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
		throw new Xcept("GGopt optimizer: " + res.termMsg);
	} 

	// native ggopt method
	private native void ggopt(
	    int threadInx,  // thread context index
	    RTContext ctxt, // evaluation context
	    int nx,   	// # pars to optimize
	    double[] x, // starting x vals, size nx, returns final vals
	    int maxfcn, // # fcn calls stop criterion 
	    int maxit, // # iteratons stop criterion 
	    double[] grdtl, // gradient stop criterion
	    double[] stptl, // relative step size stop criterion
	    double[] fmin, // min fcn value stop criterion
	    double[] eps, // relative function error 
	    double[] wk, // work area
	    int[] istop, // termination status
	    int mxdim,
	    int mxcol,
	    int mxrow,
	    double[] x0,
	    double[] h,
	    double[] s,
	    double[] theta,
	    double[] wk1,
	    double[] wk3,
	    double[] wk4
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
	public String diagInfo() { return "GGopt Optimizer"; }
	public boolean allowMP() { return false; }
}


