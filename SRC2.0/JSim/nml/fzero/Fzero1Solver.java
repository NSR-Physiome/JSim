/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// n variable linear zero finder

package JSim.nml.fzero;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import JSim.nml.*;
import Jama.*;

public class Fzero1Solver extends Solver {
	private int n; 	// # equations in problem

	// constructor
	public Fzero1Solver(SolverCallbacks cb, int inx, 
	NamedVal.Query nvals, int nn) 
	throws Xcept {
	    super(cb, inx, nvals);
	    n = nn;
	}

	// query
	public String solverName() { 
	    return "Linear zero finder";
	}

	// internal calculation
	public void solve(RTContext ctxt, 
	double[] mat, double[] vec, double[] res) throws Xcept {
            double[][] vals = new double[n][n];
            for (int i = 0; i < n; i++)
              	for (int j = 0; j < n; j++) {
                vals[i][j] = mat[i*n+j];
            }

            Matrix A = new Matrix(vals);
            Matrix b = new Matrix(vec, n);
            Matrix x = A.solve(b.times(-1));

            for (int i = 0; i < n; i++)
        	res[i] = x.get(i,0);
	}
}

