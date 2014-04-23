/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// off-line test program for optimizers
 
package JSim.tests; import JSim.nml.opt.*;

import JSim.util.*;
import JSim.expr.*; 
import JSim.jruntime.RTContext;
import JSim.data.*;

public class TestOptim {

	// mainline
	public static void main(String[] pargs) throws Exception {

	    // parse command line
	    int px = 0;
	    if (pargs[px].equals("-v")) {
	    	Util.verbose = true;
		px++;
	    }
	    if (pargs.length - px < 3) throw new Xcept(
	    	"Usage: TestOpt [-v] alg npoints x1 [ x2 ... ]");
	    String alg = pargs[px++];
	    int npts = Util.toInt(pargs[px++]);
	    int nx = pargs.length - px;
	    double[] xcent = new double[nx];
	    for (int i=0; i<nx; i++) 
	    	xcent[i] = Util.toDouble(pargs[i+px]);
	
	    // create OptimArgs
	    OptimArgs args = new OptimArgs(alg, nx);
	    for (int i=0; i<nx; i++) {
	    	args.xstart[i] = 0;
		args.xmin[i] = -10-i;
		args.xmax[i] = 10+1;
		args.xistep[i] = 0.01;
 	    }
	    args.maxCalls = 5000;
	    args.errTol = 0.001;
	    args.saveLogs = true;
	    args.stepTol = 0.001;
	    args.npoints = npts;
	    args.maxIters = 20;
	    args.gradTol = 1e-3;
	    args.eps = 1e-3;

	    // create callbacks
	    Cbs cbs = new Cbs(xcent);
	    
	    // create/call optimizer
	    System.loadLibrary("opt");
	    Optimizer.allocNativeThreads(2);
	    OptimFactory factory = new OptimFactory();
	    Optimizer opt = factory.createOptimizer(alg);
	    OptimResults res = new OptimResults(args);
	    opt.optimize(null, res, cbs);
	    
	    // print results
	    if (Util.verbose) {
		OptimReport rpt = new OptimReport(res,
		    factory.algs());
	    	System.err.println(rpt.getReport());
	    }
	    System.out.println(Util.pretty(res.bestX));
	    System.out.println("#calls=" + res.nCalls);
	    System.out.println("term=" + res.termMsg);
	}
	
	// Callbacks class
	public static class Cbs implements OptimCallbacks {
	    public double[] xcent;
	    
	    public Cbs(double[] xc) { xcent = xc; }
		
	    public double calcError(RTContext ctxt, double[] x, 
	    OptimResults res) throws Xcept {
		double err = 0;
		for (int i=0; i<x.length; i++) {
		    double d = x[i] - xcent[i];
		    err += d*d;
		}
//		Util.verbose("  call" + Util.pretty(x) + " = " + err); 
		res.addResult(err, x);
	    	return err;
	    }
	    
	    public int calcErrors(RTContext ctxt, double[][] x, 
	    double[] err, OptimResults res) 
	    throws Xcept {
	    	for (int i=0; i<err.length; i++) {
		    err[i] = calcError(ctxt, x[i], res);
		    if (err[i] < res.args.errTol) return i+1;
		}
		return err.length;
	    }

	    public SensMatrix calcSensMatrix(RTContext ctxt, 
	    double[] x, double[] dx, double[] errs, 
	    OptimResults res) throws Xcept {
	    	throw new Xcept("calcSensMat not implemented");
	    }
	}
}
