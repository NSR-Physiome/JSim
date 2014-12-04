/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// zero-finder for non-linear system of equations

package JSim.jruntime;   

import JSim.util.*;
import JSim.data.*;
import JSim.nml.*;
import JSim.nml.fzero.*;

abstract public class Fzero2Problem extends RTProblem implements Fzero2Callbacks {
	public int n; 	// # equations

	// constructor
	public Fzero2Problem (RTModel m, String na, int nn) throws Xcept {
    	    super(m, na);
	    n = nn;
	}

	// create one solver 
	public Solver createSolver(int solverInx, RTContext ctxt) 
	throws Xcept {
	    return new Fzero2Solver(
	    	this, solverInx, ctxt, n, model.optimFactory());
	}

	// solve method
	public void solve(RTContext ctxt) throws Xcept {
	    ncalls++;
	    init(ctxt);

	    // create work areas
	    double[] vmin = new double[n*n];
	    double[] vmax = new double[n];
	    double[] vguess = new double[n];
            double[] vfinal = new double[n];
	    for (int i=0; i<n; i++) {
		vmin[i] = Double.NaN;
		vmax[i] = Double.NaN;
		vguess[i] = Double.NaN;
                vfinal[i] = Double.NaN;
	    }

	    // set problem specific vmin, vmax, vstart
	    setBounds(ctxt, vmin, vmax, vguess);

	    // run zero-finder calculation - not yet implemented
	    Fzero2Solver solver = (Fzero2Solver) solver(ctxt);
	    solver.solve(ctxt, this, vguess, vmin, vmax, vfinal);

	    // export final values
	    export(ctxt, vfinal);
	}

	// query
	public String desc() { return "non-linear zero finder"; }

	// set problem specific vmin, vmax, vstart
	abstract public void setBounds(RTContext ctxt,
	double[] vmin, double[] vmax, double[] vguess) throws Xcept;

	// export final values
	abstract public void export(RTContext ctxt, double[] val) throws Xcept;

	//// Fzero2Callbacks methods

	// calc zeroes for one set of values
	abstract public void calcZero(RTContext ctxt, double[] val, double[] zero)
	throws Xcept;

	// calculate 1 error
	public double calcError(RTContext ctxt, double[] x, 
	OptimResults results) throws Xcept {
	    double[] zeroes = new double[n];
	    calcZero(ctxt, x, zeroes);
	    double tot = 0;
	    for (int i=0; i<n; i++) 
		tot += zeroes[i]*zeroes[i];
	    tot = Math.sqrt(tot);
	    tot /= n;
	    results.addResult(tot, x);
	    return tot;
	}

	// calc multiple errors (not currently parallel)
	public int calcErrors(RTContext ctxt, double[][] x, 
	double[] errs, OptimResults results) throws Xcept {
	    for (int i=0; i<errs.length; i++) {
	        errs[i] = calcError(ctxt, x[i], results);
		if (errs[i] < results.args.errTol) return i+1;
	    }
	    return errs.length;
	}

	// placeholder doesn't apply here 
	public SensMatrix calcSensMatrix(RTContext ctxt, 
	double[] x, double[] dx, 
	double[] errs, OptimResults res) throws Xcept {
	    throw new Xcept(
	    	"Fzero2Problem.calcSensMatrix() not implemented");
	}

	// get profile
	public ProfileData.Problem getProfile() {
	    ProfileData.Problem p = super.getProfile();
	    p.nstate = n;
	    p.type = ProfileData.FZERO2;
	    return p;
	}
}

