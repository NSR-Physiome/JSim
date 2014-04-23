/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// search for minimum value on parm grid, narrowing bounds each iter.
 
package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public class GridSearch extends Optimizer {
	
	// static class initialization 
	public static OptimAlg.Info algInfo() {
	    OptimAlg.Info algInfo = new OptimAlg.Info();
	    algInfo.name = "gridsearch";
	    algInfo.boundsNeeded = true;
	    algInfo.sensMatNeeded = false;
	    algInfo.parsNeeded = new String[] { "stepTol", "maxIters", "npoints" };
	    algInfo.optimClassName = GridSearch.class.getName();
	    return algInfo;
	}

	// constructor
	public GridSearch() { }

	// run optimizer
	public void optimize(RTContext ctxt, OptimResults res, 
	OptimCallbacks cbs) throws Xcept {
	
	    // initialize
	    OptimArgs args = res.args;
	    int nx = args.nx();
	    int np = args.npoints;
	    if (np < 4) throw new Xcept(this,
	    	"#points set to " + np + ", minimum in 4");
	    double[] xmin = args.xmin;
	    double[] xmax = args.xmax;
	    if (Util.hasNaNs(xmin) ||
		Util.hasNaNs(xmax)) throw new Xcept(this,
		"xmin & xmax required");
	    int ncalls = 0;
	    int niter = 0;
	    RealNData ylast = null;

	    // if xstart non-NaN,  calls those first
	    if (! Util.hasNaNs(args.xstart)) {
	    	double yval = cbs.calcError(ctxt, args.xstart, res);
		if (yval < args.errTol) {
		    term(res, "# calls");
		    return;
		}
	    }		    

	    // iteration loop: each pass narrows xmin, xmax
	    while (true) {
		Util.verbose("GridSearch iter: min=" + Util.pretty(xmin) + 
		    " max=" + Util.pretty(xmax));

		// init y with "close-enough" values from last pass, if any
	    	RealNData y = createY(args.xname, xmin, xmax, np);
		if (ylast != null) loadLast(y, ylast);
		int yct = y.nsamples();

		// count create calls needed to fill y
		int fct = 0;
		for (int i=0; i<yct; i++) 
		    if (Double.isNaN(y.realVal(i)))
		    	fct++;
		if (args.maxCalls > 0) 
		    fct = Math.min(fct, args.maxCalls - ncalls);
		
		// build x value arrays for error query
		int[] xinxs = new int[fct];
		double[][] xvals = new double[fct][nx];
		int j = 0;
		for (int i=0; i<yct; i++) {
		    if (! Double.isNaN(y.realVal(i))) continue;
		    xinxs[j] = i;
		    xvals[j] = y.gridVals(i);
		    j++;
		    if (j == fct) break;
		}
		
		// calc fct errors, building yvals
		double[] yvals = new double[fct];
		int ct = cbs.calcErrors(ctxt, xvals, yvals, res);
		ncalls += ct;

		// check stopping criteria
		if (res.bestErr < args.errTol) {
		    term(res, "mean sqr error");
		    return;
		}
		if (args.maxCalls > 0 && ncalls >= args.maxCalls) {
		    term(res, "# calls");
		    return;
		}

		// find minimum yval
		int yminx = 0;
		double ymin = 0;
		for (int i=0; i<fct; i++) {
		    double yval = yvals[i];
		    y.set(xinxs[i], yval);
		    if (i==0 || yval< ymin) {
		    	yminx = xinxs[i];
			ymin = yval;
		    }
		}
		
		// calculate new xmin, xmax from ymin
		double[] nmin = new double[nx];
		double[] nmax = new double[nx];
		int[] gpos = y.gridPos(yminx);
		for (int i=0; i<nx; i++) {
		    int minx = gpos[i]-1;
		    if (minx < 0) minx = 0;
		    nmin[i] = y.grid(i).realVal(minx);
		    int maxx = gpos[i]+1;
		    if (maxx >= np) maxx = np-1;
		    nmax[i] = y.grid(i).realVal(maxx);
		}		

		// read for new iter
		xmin = nmin;
		xmax = nmax;
		ylast = y;
		niter++;
		
		// stopping criteria
		if (args.maxIters > 0 && niter >= args.maxIters) {
		    term(res, "# iterations");
		    return;
		}
		double mstep = distance(xmin, xmax);
		if (mstep < args.stepTol) {
		    term(res, "step size");
		    return;
		}
	    }
	}

	// create new y data
	private RealNData createY(String[] xnames, double[] xmin,
	double[] xmax, int ct) throws Xcept {
	    int nx = xnames.length;
	    GridData[] grids = new GridData[nx];
	    for (int i=0; i<nx; i++) 
	    	grids[i] = new RegularGridData(xnames[i],
		    null, xmin[i], xmax[i], ct);
	    return new RealNData("y", null, grids);
	}
	
	// load "close-enough" ylast values into y		
	private void loadLast(RealNData y, RealNData ylast) 
	throws Xcept {
	    // calculate closeness thresh
	    double thresh = 0;
	    int n = y.ndim();
	    for (int i=0; i<n; i++) {
	    	RegularGridData x = (RegularGridData) y.grid(i);
		double d = x.delta();
		if (i==0 || d<thresh) thresh = d;
	    }
	    thresh *= 1e-6;		

	    // load y values for close ylast values
	    int yct = y.nsamples();
	    for (int i=0; i<yct; i++) {
		double[] gvals = y.gridVals(i);
		int linx = ylast.nearestInx(gvals);
		double[] lvals = ylast.gridVals(linx);
		double d = distance(gvals, lvals);
		if (d > thresh) continue;
		double v = ylast.realVal(linx);
//		Util.verbose("  call" + Util.pretty(gvals) + " = " + v + " linx=" + linx); 
		y.set(i, v);
	    }
	}	    

	// pythagorean distance
	private double distance(double[] x1, double[] x2) {
	    double tot = 0;
	    for (int i=0; i<x1.length; i++) {
	    	double d = x1[i] - x2[i];
	    	tot += d*d;
	    }
	    return Math.sqrt(tot);
	}

	// update OptimResults term info
	private void term(OptimResults res, String crit) {
	    res.status = OptimResults.NORMAL;
	    res.termMsg = "Met " + crit + " stopping criterion";
	}

	// query
	public String diagInfo() { return "GridSearch Optimizer"; }
	public boolean allowMP() { return true; }
}



	
	
