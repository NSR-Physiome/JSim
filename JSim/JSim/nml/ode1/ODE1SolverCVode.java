/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// CVode ODE solver

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public class ODE1SolverCVode extends ODE1Slave {

  	// Solver settings
  	private double rtol, atol; // relative and absolute error tolerance
	private int maxsteps;  // max # sub-steps in single call
  	private boolean stiff;     // use stiff method?

  	// native library load
	static {
 	    System.loadLibrary("odesolver");
  	}

	// constructor
    	public ODE1SolverCVode(ODE1Solver s) throws Xcept {
	    super(s, "cvode");
    	    rtol = realVal("ode_CVode_reltol");
    	    atol = realVal("ode_CVode_abstol");
    	    maxsteps = intVal("ode_CVode_maxsteps");
	    stiff = boolVal("ode_CVode_stiff");
  	}

	// solve method
	public void solve(RTContext ctxt, double x0, double xend, 
	double[] y0, double[] yend) throws Xcept {
	    int neqn = y0.length;
	    double[] ydot = new double[neqn];
	    for (int i = 0; i < neqn; i++)
	      yend[i] = y0[i];
	    int idid = jcvode(ctxt.threadInx, ctxt, neqn, x0, xend, 
	    	yend, ydot, rtol, atol, 
		maxsteps, stiff, callbacks());
	    if (idid < 0) throw new Xcept(
		"Solver Error: CVode - " + status(idid));
	    callbacks().evaluate(ctxt, x0, y0, ydot);
  	}

	// error message for status
	public static String status(int idid) {
	    switch (idid) {
	    case -1: return "cvode_mem argument is NULL"; 
	    case -2: return "illegal input"; 
	    case -3: return "too many internal steps"; 
	    case -4: return "unable to satisfy user-specified accuracy"; 
	    case -5: return "too many error test failures in time-step"; 
	    case -6: return "too many convergence test failures in time-step"; 
	    case -7: return "linear solver setup routine failed"; 
	    case -8: return "linear solver solve routine failed"; 
	    }
	    return "undocumented exit status=" + idid;
	}

	// native method
  	public native int jcvode(
	    int threadInx, RTContext ctxt, 
	    int neqn, double x, double xend, 
	    double[] y, double[] ydot, double rtol, double atol, 
	    int mxstep, boolean stiff,
            ODE1Callbacks h) throws Xcept;

}
