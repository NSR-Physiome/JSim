/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Provide function calls to PDE1Solver

package JSim.nml.pde1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public interface PDE1Callbacks extends SolverCallbacks, MacCormack.Callbacks {

	// solver implemented? (all solvers may not apply to all problems)
	//  see constants ASModel.PDE_*
	public boolean usesPDESolver(int which);

	////////////////// Common solver routines

	// gets user defined left boundary conditions
	public void common_LHB(RTContext ctxt, double t, double[] uc,
	double[] f1, double[] f2, double[] f3) throws Xcept;

	// gets user defined right boundary conditions
	public void common_RHB(RTContext ctxt, double t, double[] uc,
	double[] f1, double[] f2, double[] f3) throws Xcept;	

	////////////////// LSFEA solver routines

	// evaluate u:x & u:x:x coef given t, x and u
	public void LSFEA_xstep(RTContext ctxt, double t, double x,
	    double[] u,  double[] uxcoef,  double[] uxxcoef)
	    throws Xcept;

	// evaluate u:t given t, x and u 
	public void LSFEA_tstep(RTContext ctxt, double t, double x,
	    double[] u,  double[] ut) throws Xcept;
	    
	// set X domain index, return corresponding value NEW
	public double LSFEA_setX(RTContext ctxt, int xinx) 
	throws Xcept;

	////////////////// MacCormack solver routines

	// gets user defined coefficients of partial differential equations
	// of the form u:t = coefX*u:x + coefXX*u:x:x + src
	public void MacCormack_State(RTContext ctxt, double t, double x,
	double[] uc, double[] cX, double[] cXX, double[] src) throws Xcept;

	////////////////// Toms731 solver routines
	// evaluate for single x C, Q, R given u and u:x
	public void Toms731_State(RTContext ctxt, double t, double x, 
	double[] u, double[] ux, double[] C, double[] Q, double[] R) throws Xcept;

	// evalute BETA & GAMMA given u and u:x on LH boundary
	public void Toms731_LHB(RTContext ctxt, double t, double[] u, double[] ux,
	    double[] BETA, double[] GAMMA) throws Xcept;

	// evalute BETA & GAMMA given u and u:x on RH boundary
	public void Toms731_RHB(RTContext ctxt, double t, double[] u, double[] ux,
	    double[] BETA, double[] GAMMA) throws Xcept;

	// hack U0
	public void Toms731_embedU0(RTContext ctxt, double[][] u0, double[] u)
	    throws Xcept;
}

