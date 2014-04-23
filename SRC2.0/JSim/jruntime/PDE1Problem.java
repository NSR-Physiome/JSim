/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1D PDE problem with ODEs at fixed coupling points in x

package JSim.jruntime;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import JSim.nml.*;
import JSim.nml.ode1.*;
import JSim.nml.pde1.*;

abstract public class PDE1Problem extends RTProblem implements PDE1Callbacks {
	protected RTRealDomain t; // "time" domain of integration
	protected RTRealDomain x; // "space" domain of integration
	protected int nu;	// # PDEs in system
	protected int nv;	// # ODEs in system
	protected int nxcpl;	// # coupled x points
	protected RTRealNVar[] ustate; // state PDE vars u
	protected RTRealNVar[] ustatet; // state vars u:t
	protected RTRealNVar[] ustatex; // state vars u:x
	protected RTRealNVar[] ustatext; // state vars u:x:t
	protected RTRealNVar[] ustatexx; // state vars u:x:x
	protected RTRealNVar[] vstate; // state ODE vars v
	protected RTRealNVar[] vstatet; // state vars v:t

	public long[] npdeCallbacks;

	// constructor
	public PDE1Problem(RTModel m, String n) throws Xcept {
	    super(m, n);
	}

	// post-constructor setup
	//   this stuff should be in constructor,
	//	but some javac give bogus errors
	public void setup(RTRealDomain tt, RTRealDomain xx, int nnxcpl,
	RTRealNVar[] uu, RTRealNVar[] uut, RTRealNVar[] uux, 
	RTRealNVar[] uuxt, RTRealNVar[] uuxx, RTRealNVar[] vv, RTRealNVar[] vvt) 
	throws Xcept {
	    t = tt;
	    x = xx;
	    nu = uu.length;
	    nv = (vv == null) ? 0 : vv.length;
	    nxcpl = nnxcpl;
	    ustate = uu;
	    ustatet = uut;
	    ustatex = uux;
	    ustatext = uuxt;
	    ustatexx = uuxx;
	    vstate = vv;
	    vstatet = vvt;
	    npdeCallbacks = new long[ProfileData.NPDECALLBACKS];
	}

	// create one solver 
	public Solver createSolver(int solverInx, RTContext ctxt)
	throws Xcept {
	    Solver solver = new PDE1Solver(this, solverInx, ctxt, nu);
	    String[] varNames = new String[ustate.length];
	    for (int i=0; i<ustate.length; i++)
	    	varNames[i] = ustate[i].toString();
	    solver.setVarNames(varNames);
	    return solver;
	}

	// solve 1 timestep of problem
	public void solve(RTContext ctxt) throws Xcept {
	    ncalls++;
	    init(ctxt);
	    PDE1Solver solver = (PDE1Solver) solver(ctxt);

	    // nothing to do at RHB
	    if (ctxt.atRHBC(t)) return;

	    // save x state,  to restore later
	    int xstate = ctxt.getCState(x);

	    // (re)size internal arrays
	    int nx = ctxt.dct(x);
	    double[][] u0 = new double[nu][nx]; // u at t initial conditions 
	    double[][] uf = new double[nu][nx]; // u at t final state vars
	    double[][] ut0 = new double[nu][nx]; // u:t at t initial conditions
	    double[][] utf = new double[nu][nx]; // u:t at t final state vars
	    double[][] ux0 = new double[nu][nx]; // u:x at t initial conditions
	    double[][] uxf = new double[nu][nx]; // u:x at t final state vars
	    double[][] uxt0 = new double[nu][nx]; // u:x:t at t initial conditions
	    double[][] uxtf = new double[nu][nx]; // u:x:t at t final state vars
	    double[] v0 = new double[nv]; // v at t initial conditions
	    double[] vf = new double[nv]; // v at t final state vars
	    double[] vt0 = new double[nv]; // v:t at t initial conditions
	    double[] vtf = new double[nv]; // v:t at t final state vars
	    double[] xcpl = new double[nxcpl]; // xcoupling values

	    // initialize internal arrays
	    for (int i=0; i<nu; i++) {
		RTRealNVar u = ustate[i];
		for (int j=0; j<nx; j++) {
		    ctxt.setDInx(x, j);
		    u0[i][j] = ctxt.realVal(u);
		    uf[i][j] = Double.NaN;
		    ut0[i][j] = Double.NaN;
		    utf[i][j] = Double.NaN;
		    ux0[i][j] = Double.NaN;
		    uxf[i][j] = Double.NaN;
		    uxt0[i][j] = Double.NaN;
		    uxtf[i][j] = Double.NaN;
		}
	    }
	    for (int i=0; i<nv; i++) {
		RTRealNVar v = vstate[i];
		v0[i] = ctxt.realVal(v);
		vf[i] = Double.NaN;
		vt0[i] = Double.NaN;
		vtf[i] = Double.NaN;
	    }
	    for (int i=0; i<nxcpl; i++)
		xcpl[i] = Double.NaN;

	    // get ODE/PDE coupling x values (if any)
	    if (nxcpl>0) calcXcpl(xcpl);

	    // if IC, check IC/BC consistency
	    if (ctxt.atLHBC(t)) {
		double u[] = new double[nu];
		double ux[] = new double[nu];
		double beta[] = new double[nu];
		double gamma[] = new double[nu];

		// LHBC checks
		ctxt.setLHBC(x);
		for (int i=0; i<nu; i++) {
		    u[i] = u0[i][0];
		    ux[i] = Double.NaN;
		}
		Toms731_LHB(ctxt, ctxt.realVal(t), u, ux, beta, gamma);
		for (int i=0; i<nu; i++) 
		    // beta=0 -> no u:x dependence,  gamma=f(u)
		    //    if gamma!=NaN && non-zero,  inconsistent
		    if (beta[i] == 0 && !Double.isNaN(gamma[i]) && gamma[i] != 0) 
			model.badICBC(
			    "IC/LHBC inconsistency for state var " + 
			    ustate[i], ustate[i], gamma[i]);

		// RHBC checks
		ctxt.setRHBC(x);
		for (int i=0; i<nu; i++) u[i] = u0[i][nx-1];
		Toms731_RHB(ctxt, ctxt.realVal(t), u, ux, beta, gamma);
		for (int i=0; i<nu; i++) 
		    if (beta[i] == 0 && !Double.isNaN(gamma[i]) && gamma[i] != 0)  
			model.badICBC(
			    "IC/RHBC inconsistency for state var " + 
			    ustate[i], ustate[i], gamma[i]);
	    }

	    // call NML solver 
   	    double t0 = ctxt.realVal(t);
	    double tf = t0 + ctxt.cdelta(t);
	    solver.solve(ctxt, t0, tf, ctxt.grid(x).samples(), u0, uf, ut0, utf, ux0, uxf, uxt0, uxtf,
		v0, vf, vt0, vtf);

	    // at LHBC,  update initial deriv values
	    if (ctxt.atLHBC(t)) 
		saveArrays(ctxt, null, ut0, ux0, uxt0, null, vt0);

	    // save state & derivs variables for t+t.delta
            ctxt.moveRight(t);
	    saveArrays(ctxt, uf, utf, uxf, uxtf, vf, vtf);
	    ctxt.moveLeft(t);

	    // restore x state
	    ctxt.setCState(x, xstate);
	}

	// save var arrays at timepoint (may be initial or final)
	private void saveArrays(RTContext ctxt, 
	double[][] u, double[][] ut, double[][] ux, 
	double[][] uxt, double[] v, double[] vt) 
	throws Xcept {
	    for (int i=0; i<nu; i++) {
		RTRealNVar uu = (u == null) ? null : ustate[i];
		RTRealNVar uut = ustatet[i];
		RTRealNVar uux = ustatex[i];
		RTRealNVar uuxt = (ustatext == null) ? null : ustatext[i];
	    	int nx = ctxt.dct(x);
		for (int j=0; j<nx; j++) {
		    ctxt.setDInx(x, j);
		    if (uu != null) ctxt.set(uu, u[i][j]);
		    if (uut != null) ctxt.set(uut, ut[i][j]);
		    if (uux != null) ctxt.set(uux, ux[i][j]);
		    if (uuxt != null) ctxt.set(uuxt, uxt[i][j]);
		}
	    }
	    for (int i=0; i<nv; i++) {
		RTRealNVar vv = (v == null) ? null : vstate[i];
		RTRealNVar vvt = vstatet[i];
		if (vv != null) ctxt.set(vv, v[i]);
		ctxt.set(vvt, vt[i]);
	    }
	}  		

	// calculate ODE/PDE coupling x values (if any)
	//    override in subclasses if appropriate
	public void calcXcpl(double[] xcpl) throws Xcept {
	    throw new Xcept(this, "calcXcpl() not implemented");
	}

	// call BCs in more convenient form from solver
        public void Toms731_BC(RTContext ctxt, int left, double t, double[] u, double[] ux,
	    double[] BETA, double[] GAMMA) throws Xcept {
            if (left == 1) 
            	Toms731_LHB(ctxt, t, u, ux, BETA, GAMMA);
            else
                Toms731_RHB(ctxt, t, u, ux, BETA, GAMMA);
	}

	// query
	public String desc() { return "PDE solver"; }
        public RTRealNVar[] getStatevars() {  return ustate;  }

	///////// Placeholder methods to override in subclasses (see PDE1Callbacks)

	// placeholder for common solver routines
	public void common_LHB(RTContext ctxt, double t, double[] uc,
	double[] f1, double[] f2, double[] f3) throws Xcept {
	    throw new Xcept(this, "common_LHB not implemented");
	}
	public void common_RHB(RTContext ctxt, double t, double[] uc,
	double[] f1, double[] f2, double[] f3) throws Xcept {
	    throw new Xcept(this, "common_RHB not implemented");
	}

	// placeholder LSFEA solver routines 
	public void LSFEA_tstep(RTContext ctxt, double t, double x,
	    double[] u,  double[] ut) throws Xcept {
	    throw new Xcept(this, "LSFEA_tstep not implemented");
	}
	public void LSFEA_xstep(RTContext ctxt, double t, double x,
	    double[] u,  double[] uxcoef, double[] uxxcoef) 
	    throws Xcept {
	    throw new Xcept(this, "LSFEA_xstep not implemented");
	}

	// set x index, return value
	public double LSFEA_setX(RTContext ctxt, int xinx) throws Xcept {
	    ctxt.setDInx(x, xinx);
	    return ctxt.realVal(x);
	}

	// placeholder MacCormack solver routines
	public void MacCormack_State(RTContext ctxt, double t, double x,
	double[] uc, double[] cX, double[] cXX, double[] src) throws Xcept {
	    throw new Xcept(this, "MacCormack_State not implemented");
	}

	// placeholder Toms731 solver routines 
	public void Toms731_State(RTContext ctxt, double t, double x, double[] u, double[] ux,
	    double[] C, double[] Q, double[] R) throws Xcept {
	    throw new Xcept(this, "Toms731_CQR not implemented");
	}
	public void Toms731_LHB(RTContext ctxt, double t, double[] u, double[] ux,
	    double[] BETA, double[] GAMMA) throws Xcept {
	    throw new Xcept(this, "Toms731_LHB not implemented");
	}
	public void Toms731_RHB(RTContext ctxt, double t, double[] u, double[] ux,
	    double[] BETA, double[] GAMMA) throws Xcept {
	    throw new Xcept(this, "Toms731_RHB not implemented");
	}

	// embed a flat version of u0 in larger native array
	//    this sucks,  should reenginneer! 
	public void Toms731_embedU0(RTContext ctxt, 
	double[][] u0, double[] u) throws Xcept {
            int nx = ctxt.dct(x);
            for (int i=0; i<nu; i++) 
            	for (int j=0; j<nx; j++) 
              	    u[i+j*(1+nu)] = u0[i][j];
        }

	// get profile
	public ProfileData.Problem getProfile() {
	    ProfileData.Problem p = super.getProfile();
	    p.name = ustate[0].name();
	    p.nstate = ustate.length;
	    p.type = ProfileData.PDE;
	    for (int i=0; i<npdeCallbacks.length; i++)
	    	p.ncallbacks += npdeCallbacks[i];
	    p.npdeCallbacks = (long []) npdeCallbacks.clone();
	    return p;
	}
}

