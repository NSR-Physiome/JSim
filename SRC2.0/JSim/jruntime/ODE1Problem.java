/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1st degree ODE problem

package JSim.jruntime;

import JSim.util.*;
import JSim.data.*;
import JSim.nml.*;
import JSim.nml.ode1.*;
import JSim.nml.pde1.*;

abstract public class ODE1Problem extends RTProblem implements ODE1Callbacks {
	protected int n;	// # ODEs in systems
	protected RTRealDomain t; // domain of integration
	protected RTRealNVar[] vstate; // state vars

	// constructors
	public ODE1Problem(RTModel m, String n) throws Xcept {
	    super(m, n);
	}
	
	// post-constructor setup
	//   this stuff should be in constructor,
	//	but some javac give bogus errors
	public void setup(RTRealDomain tt, RTRealNVar[] vv) throws Xcept {
	    n = vv.length;
	    t = tt;
	    vstate = vv;
	}

	// create one solver 
	public Solver createSolver(int solverInx, RTContext ctxt)
	throws Xcept {
	    return new ODE1Solver(this, solverInx, ctxt, n);
	}

	// solve the problem
	public void solve(RTContext ctxt) throws Xcept {
	    ncalls++;   // increment profile 
	    init(ctxt);
	    double[] u0 = new double[n]; // initial conditions
	    double[] uf = new double[n]; // final state vars
	    double[] udot0 = new double[n]; // u:t at t0
	    for (int i=0; i<n; i++) {
		u0[i] = Double.NaN;
		uf[i] = Double.NaN;
		udot0[i] = Double.NaN;
	    }
	    ODE1Solver solver = (ODE1Solver) solver(ctxt);
	    for (int i=0; i<vstate.length; i++)
		u0[i] = ctxt.realVal(vstate[i]);
            if (! ctxt.atRHBC(t)) {
		double t0 = ctxt.realVal(t);
		double tf = t0 + ctxt.cdelta(t);
		solver.solve(ctxt, t0, tf, u0, uf);

		// save state vars at tf
                ctxt.moveRight(t);
		for (int i=0; i<vstate.length; i++)
		    ctxt.set(vstate[i], uf[i]);
		ctxt.moveLeft(t);

	    }
	}

	// interrupt check
	//    terminate if t=NaN
	//    use ut[0]=NaN as terminate flag to native solvers
	public void interruptCheck(RTContext ctxt, double t, double[] ut) 
	throws Xcept {
	    if (Double.isNaN(t) || ctxt.cancelOrSkip())
	    	for (int i=0; i<ut.length; i++)
		    ut[i] = Double.NaN;
	    if (Double.isNaN(t))
	    	throw new Xcept("ODE evaluation aborted: t=NaN");
	    ctxt.interruptCheck();
	}
	
	// calculate udot,  given u at t
	abstract public void evaluate(RTContext ctxt, 
	double tt, double[] u, double[] udot) throws Xcept;

	// query
	public String desc() { return "ODE solver"; }
        public RTRealNVar[] getStatevars() {
            return vstate;
        }

	// get profile
	public ProfileData.Problem getProfile() {
	    ProfileData.Problem p = super.getProfile();
	    p.name = vstate[0].name();
	    p.nstate = vstate.length;
	    p.type = ProfileData.ODE;
	    return p;
	}
}

