/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

 // zero-finder for linear system of equations

package JSim.jruntime;   

import JSim.util.*;
import JSim.data.*;
import JSim.nml.*;
import JSim.nml.fzero.*;

abstract public class Fzero1Problem extends RTProblem 
implements SolverCallbacks {
	public int n; 	// # equations

	// constructor
	public Fzero1Problem (RTModel m, String na, int nn) throws Xcept {
    	    super(m, na);
	    n = nn;
	}

	// create one solver 
	public Solver createSolver(int solverInx, RTContext ctxt)
	throws Xcept {
	    return new Fzero1Solver(this, solverInx, ctxt, n);
	}

	// solve method
	public void solve(RTContext ctxt) throws Xcept {
	    ncalls++;
	    ncallbacks++;
	    init(ctxt);
	    double[] mat = new double[n*n];
	    double[] vec = new double[n];
	    double[] res = new double[n];
	    for (int i=0; i<n*n; i++) mat[i] = Double.NaN;
	    for (int i=0; i<n; i++) {
		vec[i] = Double.NaN;
		res[i] = Double.NaN;
	    }
	    setCoef(ctxt, mat, vec, res);
	    Fzero1Solver solver = (Fzero1Solver) solver(ctxt);
	    solver.solve(ctxt, mat, vec, res);
	    export(ctxt, res);
	}

	// query
	public String desc() { return "linear zero finder"; }

	// set matrix coef
	abstract public void setCoef(RTContext ctxt,
	double[] mat, double[] vec, double[] res) throws Xcept;

	// export final values
	abstract public void export(RTContext ctxt, double[] val)
	throws Xcept;

	// get profile
	public ProfileData.Problem getProfile() {
	    ProfileData.Problem p = super.getProfile();
	    p.nstate = n;
	    p.type = ProfileData.FZERO1;
	    return p;
	}
}

