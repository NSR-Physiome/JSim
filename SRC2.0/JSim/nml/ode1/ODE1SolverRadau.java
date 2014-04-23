/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Radau ODE solver

package JSim.nml.ode1;

import JSim.nml.*;
import JSim.util.*;
import JSim.jruntime.RTContext;

public class ODE1SolverRadau extends ODE1Slave {

  	// Solver settings
  	private double rtol, atol; // relative and absolute error tolerance
  	private int nmax;          // maximum number of steps, 100000
  	private int nsmin;         // minimum order, 3
  	private int nsmax;         // maximum order, 7
  	private int nsini;         // initial order, 3
  	private int snewton;       // parameter for selecting the starting value of
  	                           // Newton interations: 0 means zero starting value
  	                           // non-zero value means using the extrapolated collocation solution
  	private double uround;     // the rounding unit or
  	                           // the smallest number to satisfying 1.0+round > 1.0, 2.3e-16
  	private double safe;       // the safety factor in step size prediction, 0.9
  	private double compjac;    // parameter of how frequently Jacobian should be recomputed
  	                           // -1 forces recomputation; smaller number (between 0.001 ~ 0.1)
  	                           // means more frquently, default is 0.001
  	private double fac1, fac2; // step size is not changed if
  	                           // fac1 < HNEW/HOLD < fac2, 1, 1.2
  	private double fac3, fac4; // parameters for step size selection, 0.2, 8
  	                           // fac3 <= HNEW/HOLD <= fac4
  	private double fac5;       // order is increased if contractivity factor is
  	                           // less than this parameter, 0.002
  	private double fac6;       // order is decreased if contractivity factor is
  	                           // larger than this parameter, 0.8
  	private double fac7, fac8; // order is decreased only if fac8 <= HNEW/H <= fac7
  	                           // 1.2, 0.8

  	// native library load
	static {
	    System.loadLibrary("odesolver");
  	}

	// constructor
    	public ODE1SolverRadau(ODE1Solver s) throws Xcept {
	    super(s, "Radau (stiff)");
	    rtol = realVal("ode_Radau_reltol");
	    atol = realVal("ode_Radau_abstol");
	    nmax = intVal("ode_Radau_nstep");
	    uround = intVal("ode_Radau_round");
	    safe = realVal("ode_Radau_safety");
	    nsmin = intVal("ode_Radau_minord");
	    nsmax = intVal("ode_Radau_maxord");
	    nsini = intVal("ode_Radau_initord");
	    snewton = intVal("ode_Radau_newton");
	    compjac = realVal("ode_Radau_jacob");
	    fac1 = realVal("ode_Radau_losize");
	    fac2 = realVal("ode_Radau_hisize");
	    fac3 = realVal("ode_Radau_loselect");
	    fac4 = realVal("ode_Radau_hiselect");
	    fac5 = realVal("ode_Radau_locontract");
	    fac6 = realVal("ode_Radau_hicontract");
	    fac7 = realVal("ode_Radau_hiorder");
	    fac8 = realVal("ode_Radau_loorder");
  	}

	// solve method
  	public void solve(RTContext ctxt, double x0, double xend, 
	double[] y0, double[] yend) throws Xcept {
	    int neqn = y0.length;
	    double[] ydot = new double[neqn];

	    for (int i = 0; i < neqn; i++) {
	      if (Double.isInfinite(y0[i]) || Double.isNaN(y0[i]))
	        throw new Xcept("Solver Error: Radau, state variables are either Inf or NaN");
	      yend[i] = y0[i];
	    }

	    int idid = jradau(ctxt.threadInx, ctxt, neqn, x0, xend, 
	    	  yend, ydot, rtol, atol, nmax, nsmin, nsmax, nsini,
	          snewton, uround, safe, compjac, fac1, fac2, fac3, fac4, fac5, fac6,
	          fac7, fac8,callbacks());

	    if (idid < 0) {
	      throw new Xcept("Solver Error: Radau - " + status(idid));
	    }

	    callbacks().evaluate(ctxt, x0, y0, ydot);
  	}

	// native solver
  	public native int jradau(
	    int threadInx, RTContext ctxt,
	    int neqn, double x, double xend, 
	    double[] y, double[] ydot, double rtol, double atol,
            int nmax, int nsmin, int nsmax, int nsini,
            int snewton, double uround, double safe,
            double compjac, double fac1, double fac2,
            double fac3, double fac4, double fac5, double fac6,
            double fac7, double fac8, ODE1Callbacks h);

	// error message for status
	public static String status(int idid) {
	    switch (idid) {
	    case -1: return "input is not consistent"; 
	    case -2: return "larger internal nmax array needed"; 
	    case -3: return "step size becomes too small"; 
	    case -4: return "internal matrix is repeatedly singular"; 
	    }
	    return "undocumented exit status=" + idid;
	}
}
