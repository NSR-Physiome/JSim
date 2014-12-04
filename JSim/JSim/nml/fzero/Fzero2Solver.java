/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// n-dimensional non-linear zero finder

package JSim.nml.fzero;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import JSim.nml.*;
import JSim.nml.opt.*;

public class Fzero2Solver extends Solver {
	private int n; // # equations in problem
	private String unboundAlg, boundAlg;
	private OptimFactory optimFactory;
	private Optimizer unboundOpt, boundOpt;
	private OptimArgs args;

	// constructor
	public Fzero2Solver(Fzero2Callbacks cb, int inx, 
	NamedVal.Query nvals, int nn,
	OptimFactory factory)
	throws Xcept {
	    super(cb, inx, nvals);
	    n = nn;
	    optimFactory = factory;

	    // create bound/unbound optimizers
	    unboundAlg = stringVal("fzero_unbound");
	    unboundOpt = optimFactory.createOptimizer(unboundAlg);
	    boundAlg = stringVal("fzero_bound");
	    boundOpt = (boundAlg.equals(unboundAlg)) ? 
	        unboundOpt : optimFactory.createOptimizer(boundAlg);

	    // initial arguments
	    args = new OptimArgs(unboundAlg, n);
	    for (int i=0; i<n; i++) {
	        args.xname[i] = "x" + i;
	        args.xistep[i] = realVal("fzero_istep");
	    }
	    args.maxCalls = n * intVal("fzero_maxcalls");
	    args.errTol = realVal("fzero_errtol");
	    args.stepTol = 0; // don't stop for small step size
	    args.gradTol = 0; // don't stop for small gradient
	    args.saveLogs = false;
	    args.maxIters = n * intVal("fzero_maxiters");
	    args.eps = realVal("fzero_eps");
	    args.npoints = intVal("fzero_npoints");
	    args.randomSeed = intVal("fzero_randseed");
	    args.initTemp = realVal("fzero_inittemp");
	    args.populationSize = intVal("fzero_populationsize");
	    args.mutationRate = realVal("fzero_mutationrate");
	    args.crossoverRate = realVal("fzero_crossoverrate");
	    args.mutationStep = realVal("fzero_mutationstep");
	    args.eliteCutoff = realVal("fzero_elitecutoff");
	    args.selectMethod = intVal("fzero_selectmethod"); 
	}

	// query
	public String solverName() { 
	    return "Non-linear zero finder";
	}

	// internal calculation
	public void solve(RTContext ctxt, Fzero2Callbacks cbs, 
	double[] guess, double[] min,
	double[] max, double[] result) throws Xcept {

	    // which optimizer (bound or unbound)
	    Optimizer opt = boundOpt;
	    for (int i=0; i<n; i++) {
	        if (! Double.isNaN(min[i])) continue;
	        if (! Double.isNaN(max[i])) continue;
	        opt = unboundOpt;
	        break;
	    }

	    // bulletproof initialize guesses
	    for (int i=0; i<n; i++) {
	        if (Double.isNaN(guess[i]))
	            guess[i] = (max[i]+min[i])/2;
	        if (Double.isNaN(guess[i]))
	            guess[i] = 1; // good as any, I guess
	    }

	    // update args, create results table
	    args.alg = (opt == boundOpt) ? boundAlg : unboundAlg;
	    args.xstart = guess;
	    args.xmin = min;
	    args.xmax = max;
	    OptimResults results = new OptimResults(args);

	    // run optimizer
	    opt.optimize(ctxt, results, cbs);

	    // copy back results
	    for (int i=0; i<n; i++) 
	        result[i] = results.bestX[i];

	    // check if sufficient agreement
	    double err = cbs.calcError(ctxt, result, results);
	    if (err <= args.errTol) return; // HACK!!!
	    throw new Xcept(
	        "Variables " + cbs.solvedVarsText() + 
	        " exceeded specified zero-finder error: " +
	        " (" + err + ">" + args.errTol + ")");
	}
}
