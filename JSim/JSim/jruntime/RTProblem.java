/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time solver problem

package JSim.jruntime;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.nml.Solver;
import java.util.ArrayList;

abstract public class RTProblem implements DiagInfo {
	protected RTModel model;
	protected String name;
	protected ASVar[] vsol;
	protected Solver[][] solvers; // [threadInx][loopDomainInx]
	protected RTRealDomain[] loopDomains; // extra domain loops

	// profiling info
	protected long ncalls; // # problem calls for profile
	public long ncallbacks; // # total callbacks for profile

	// constructors
	public RTProblem(RTModel m, String n) throws Xcept {
	    model = m;
	    name = n;
	    model.add(this);
	}

	// solved variables for diagnostic info
	public ASVar[] getSolvedVars() { return vsol; }
	public void setSolvedVars(ASVar[] v) { vsol = v; }
	public String solvedVarsText() {
	    if (vsol == null) return "{}";
	    StringBuffer buf = new StringBuffer("{");
	    for (int i=0; i<vsol.length; i++) {
		if (i>0) buf.append(",");
		buf.append(vsol[i].name());
	    }
	    buf.append("}");
	    return buf.toString();
	}

	// domains this problem loops over 
	public RTRealDomain[] loopDomains() { return loopDomains; }

	// allocate solver space for MP run job
	protected void allocThreads(int n) throws Xcept {
	    solvers = new Solver[n][0];
	}

	// prepare for single or multiple runs
	protected void runPrep(RTContext ctxt) throws Xcept {
	    solvers[ctxt.threadInx] = new Solver[0];
	}

	// init this context
	public void init(RTContext ctxt) throws Xcept {
	    int tx = ctxt.threadInx;
	    if (solvers[tx].length != 0) return;
	
	    // create one solver to check for reentrancy
	    Solver s0 = createSolver(0, ctxt);

	    // calc solver array length for this threadInx using loopDomains
	    int lct = 1;
	    if (! s0.isReentrant() && loopDomains() != null) {
	    	RTRealDomain[] doms = loopDomains();
	    	for (int i=0; i<doms.length; i++) 
		    lct *= ctxt.dct(doms[i]);
	    }

	    // create/init solver array for this tx
	    solvers[tx] = new Solver[lct];
	    solvers[tx][0] = s0;
	    for (int i=1; i<lct; i++) 
		solvers[tx][i] = createSolver(i, ctxt);
	}

	// solver for current pass
	public Solver solver(RTContext ctxt) throws Xcept {
	    int tx = ctxt.threadInx;
	    RTRealDomain[] doms = loopDomains();
	    if (solvers[tx].length == 1)
	    	return solvers[tx][0];
	    if (solvers[tx].length == 0)
	    	throw new Xcept(this, "No solver array for tx=" + 
		tx + ": no init()?");

	    // loopDomains index
	    int lx = 0;
	    if (doms != null) {
	    	lx = ctxt.dinx(doms[0]);
	    	for (int i=1; i<doms.length; i++) 
		    lx = lx * ctxt.dct(doms[i]) + ctxt.dinx(doms[i]);
	    }
	    return solvers[tx][lx];
	}

	// create one solver 
	abstract Solver createSolver(int solverInx, RTContext ctxt) throws Xcept;

	// profile support
	public void clearProfile() {
	    ncalls = 0;
	    ncallbacks = 0;
	}
	
	// common profile info (override in subclasses)
	public ProfileData.Problem getProfile() {
	    ProfileData.Problem p = new ProfileData.Problem();
	    int inx = name.indexOf('_');
	    while (inx > 0 && inx<name.length()-2 
	    && name.charAt(inx+1) == '_')
	    	inx++;
	    p.name = (inx > 0) ? name.substring(inx+1) : name;
	    p.ncalls = ncalls;
	    p.ncallbacks = ncallbacks;
	    return p;
	}

	// query
	public abstract String desc(); // description of type of problem
	public String diagInfo() { return desc(); }
	public String name() { return name; }
	public RTModel model() { return model; }
	abstract public void solve(RTContext ctxt) throws Xcept;

	// list of problems
	public static class List extends ArrayList<RTProblem> {
            public List(int i) { super(i); }
            public RTProblem prob(int i) { return (RTProblem) get(i); }
        }
}
