/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// adaptive ODE solver (dopri5/radau)

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;
import JSim.nml.ode1.*;

public class ODE1SolverAuto extends ODE1Slave {
	private ODE1SolverDopri5 dopri5;
	private ODE1SolverRadau  radau;
  	private boolean useRadau; // which to use


	// constructor
    	public ODE1SolverAuto(ODE1Solver s) throws Xcept {
	    super(s, "Adaptive (Dopri5/Radau)");
	    dopri5 = new ODE1SolverDopri5(s);
	    radau  = new ODE1SolverRadau(s);
	    useRadau = false;
  	}

	// solve method
	public void solve(RTContext ctxt, double x0, double xend, 
	double[] y0, double[] yend) throws Xcept {
	    int neqn = y0.length;
	    double[] ydot = new double[neqn];
	    for (int i = 0; i < neqn; i++)
	        yend[i] = y0[i];

	    // try dopri,  switch if seems stiff
	    if (! useRadau) {
	      	int idid = dopri5.solve0(ctxt, x0, xend, y0, yend);
		switch (idid) {
		case -2:
		case -3:
		case -4:
		case -5:
		    useRadau = true;
		    break;
		case -1:
		    throw new Xcept("Solver Error: Dopri5 - " +
			dopri5.status(idid));
		}
	    }

	    // try radau?
	    if (useRadau) 
	      	radau.solve(ctxt, x0, xend, y0, yend);
	}
}
