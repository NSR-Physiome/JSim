/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Dopri5 ODE solver

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public class ODE1SolverDopri5 extends ODE1Slave {

  	// Solver settings
  	private double rtol, atol; // relative and absolute error tolerance
  	private long nmax;         // maximum number of steps, 100000
  	private long nstiff;       // parameter for stiffness detection, 1000 or nmax+10
  	private double uround;     // the rounding unit or
  	                           // the smallest number to satisfying 1.0+round > 1.0, 2.3e-16
  	private double safe;       // the safety factor in step size prediction, 0.9
  	private double fac1, fac2; // parameters for step size selection, 0.2, 10
  	                           // fac1 <= HNEW/HOLD <= fac2
  	private double beta;       // beta for step control stabilization, 0.04
  	                           // larger number of beta (<=0.1) makes step control more stable

  	// native library load
	static {
	    System.loadLibrary("odesolver");
  	}

	// constructor
    	public ODE1SolverDopri5(ODE1Solver s) throws Xcept {
	    super(s, "Dopri5 (non-stiff)");
    	    rtol = realVal("ode_Dopri5_reltol");
    	    atol = realVal("ode_Dopri5_abstol");
	    nmax = intVal("ode_Dopri5_nstep");
	    nstiff = intVal("ode_Dopri5_stiff");
	    uround = realVal("ode_Dopri5_round");
	    safe = realVal("ode_Dopri5_safety");
	    fac1 = realVal("ode_Dopri5_loselect");
	    fac2 = realVal("ode_Dopri5_hiselect");
	    beta = realVal("ode_Dopri5_beta");
  	}

	// solve method
	public void solve(RTContext ctxt, double x0, double xend, 
	double[] y0, double[] yend) throws Xcept {
	    int idid = solve0(ctxt, x0, xend, y0, yend);
	    if (idid < 0) throw new Xcept(
		"Solver Error: Dopri5 - " + status(idid));
 	}

	// error message for status
	public static String status(int idid) {
	    switch (idid) {
	    case -1: return "input is not consistent"; 
	    case -2: return "larger internal nmax array needed"; 
	    case -3: return "step size becomes too small"; 
	    case -4: return "problem is probably too stiff"; 
	    case -5: return "NaNs detected during evaluation"; 
	    }
	    return "undocumented exit status=" + idid;
	}

	// solve sub-routine
	public int solve0(RTContext ctxt, double x0, double xend, 
	double[] y0, double[] yend) throws Xcept {
	    int neqn = y0.length;
	    double[] ydot = new double[neqn];
	    for (int i = 0; i < neqn; i++)
	      yend[i] = y0[i];
	    int idid = jdopri5(ctxt.threadInx, ctxt, neqn, x0, xend, 
	    	yend, ydot, rtol, atol, nmax,
                 nstiff, uround, safe, fac1, fac2, beta, callbacks());
	    callbacks().evaluate(ctxt, x0, y0, ydot);
	    return idid;
  	}

	// native method
  	public native int jdopri5(
	    int threadInx, RTContext ctxt, 
	    int neqn, double x, double xend, double[] y,
            double[] ydot, double rtol, double atol, long nmax,
            long nstiff, double uround, double safe, double fac1,
            double fac2, double beta, ODE1Callbacks h) throws Xcept;

}
