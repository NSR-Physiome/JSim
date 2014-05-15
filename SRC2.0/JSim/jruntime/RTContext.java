/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time variable context

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.fgen.*;
import JSim.aserver.*;

public class RTContext extends FgenContext implements NamedVal.Query {
	public RTModel model;
	public int threadInx; // for native threads
	private int nproc;  // # processors allocated to this ctxt
	public RTDataStore store;
	private boolean isQuery; // context for external query?
	private int[] cstate;   // computational state, by domain
	    // -1 = unSet, >=0 = domain index
	private int[] dstate;   // data state, by domain
	    // -1 = unSet, >=0 = domain index
	private static int UNSET = -1;

	// constructor
	public RTContext(RTModel m, int thread, int np, 
	RTDataStore s, boolean q) {
	    super(JSLang.lang);
	    model = m;
	    threadInx = thread;
	    nproc = np;
	    store = s;
	    isQuery = q;
	    cstate = new int[model.domains.size()];
	    dstate = new int[model.domains.size()];
	    for (int i=0; i<cstate.length; i++) 
		cstate[i] = dstate[i] = UNSET;
	}
	public RTContext(RTContext ctxt, RTDataStore s) {
	    super(JSLang.lang);
	    model = ctxt.model;
	    threadInx = ctxt.threadInx;
	    nproc = 1;
	    store = s;
	    isQuery = ctxt.isQuery;
	    cstate = (int[]) ctxt.cstate.clone();
	    dstate = (int[]) ctxt.dstate.clone();
	}   
	
	// run-time interrupt check
	public void interruptCheck() throws Xcept {
	    if (model.cancelRun) 
	        throw new Xcept("Model run canceled");
	    if (store.skipRun) 
	    	throw new Xcept("Model run skipped");		
	}

	// skip or cancel?
	public boolean cancelOrSkip() {
	    return model.cancelRun || store.skipRun;
	}

	// simple query
	public RTDataStore store() { return store; }
	public boolean isQuery() { return isQuery; }
	public String newName(Named n) {
	    return n.name();
	}
	public String funcCall(Named n, Expr.List elist) {
	    return n.name() + elist.toString(this);
	}

	// set Domain parameters
	protected void setDomain(RTRealDomain x) throws Xcept {
	    int cct = intVal(x.vct);
	    if (cct < 2) throw new Xcept(x,
		"Attempt to set domain grid ct < 2");
	    double cmin = realVal(x.vmin);
	    double cmax = realVal(x.vmax);
	    GridData cgrid = new RegularGridData(
		x.name(), x.unit(), cmin, cmax, cct);
	    double cdelta = (cmax-cmin)/(cct-1);
	    int xid = x.domainID();
	    int nth = getRuntimeNth(x);
	    if (cct <= nth) nth = 1;
	    int dct = (cct+nth-1)/nth;
	    int flop = (cct-1) % nth;
	    double dmin = cmin + flop*cdelta;
	    double dmax = cmax;
	    GridData dgrid = new RegularGridData(
		x.name(), x.unit(), dmin, dmax, dct);
	    store().setDomainData(x, dgrid, cgrid, nth, flop);
	    if (RTModel.NTHPOST && getPostNth(x) > 1)
	    	store().createStoreNth();
	    unSet(x); // must be set before access
	}

	// get Nth value to be used during model run
	private int getRuntimeNth(RTRealDomain x) throws Xcept {
	    return RTModel.NTHMIDRUN ? getNth(x) : 1;
	}
	
	// get Nth value to be used for post-processing 
	protected int getPostNth(RTRealDomain x) throws Xcept {
	    return RTModel.NTHPOST ? getNth(x) : 1;
	}
	
	// get user-specified nth value for a domain
	private int getNth(RTRealDomain x) throws Xcept {
	    NamedVal nval = namedVal("memory.storeGrids");
	    if (nval == null || nval.intVal() != ASModel.MEMORY_GRID_NTH) 
	    	return 1;
	    String s = "memory." + x + ".nth";
	    nval = namedVal(s);
	    if (nval == null) return 1;
	    int nth = nval.intVal();
	    if (nth < 1) nth = 1;
	    int xct = intVal(x.vct);
	    if (nth >= xct) nth = xct-1; // assure at least 2 grid pts
	    return nth;
	}

	// FgenContext 
	public NamedExpr domain(String n) throws Xcept {
	    RTVar v = model.getVar(n);
	    if (! v.isDomain()) throw new Xcept(v,
	    	"Variable is not domain as required");
	    return v;
	}
	public GridData gdata(NamedExpr x) throws Xcept {
	    return grid((RTRealDomain) x);
	}
	public NamedVal namedVal(String n) throws Xcept {
	    return store.namedVal(n);	    
	}

	// domain data query
	public GridData grid(RTRealDomain x) throws Xcept {
	    GridData grid = store.gdata(x);
	    if (grid == null) throw new Xcept(x,
		"Data grid not yet available");
	    return grid;
	}
	public int cct(RTRealDomain x) {
	    return store.cct(x);
	}
	public int dct(RTRealDomain x) throws Xcept {
	    return grid(x).ct();
	}
	public double cdelta(RTRealDomain x) throws Xcept {
	    return x.vdelta.realVal(this);
	}
	public double realVal(RTRealDomain x) throws Xcept {
	    return store.cgrid(x).realVal(cinx(x));
	}

	// domain state query
	public int getCState(RTRealDomain x) throws Xcept {
	    return cstate[x.domainID()];
	}
	public int cinx(RTRealDomain x) throws Xcept {
 	    int inx = cstate[x.domainID()];
	    if (inx<0) throw new Xcept(x, 
		"Domain index not set");
	    return inx; 
	}
	public int dinx(RTRealDomain x) throws Xcept {
 	    int inx = dstate[x.domainID()];
	    if (inx<0) throw new Xcept(x, 
		"Domain index not set");
	    return inx; 
	}
	public int nth(int xid) { 
	    return store.nth(xid);
	}

	public boolean atLHBC(RTRealDomain x) throws Xcept { 
	    return (getCState(x) == 0); 
	}
	public boolean atRHBC(RTRealDomain x) throws Xcept { 
	    return cinx(x) == cct(x)-1;
	}
	public boolean isSet(RTRealDomain x) throws Xcept { 
	    return (getCState(x) >= 0); 
	}

	// convert between cinx & dinx once domain is set
	private int cinx2dinx(int xid, int cinx) {
	    if (cinx<0) return UNSET;
	    int flop = store.flop(xid);
	    int nth = store.nth(xid);
	    return (cinx+nth-1-flop)/nth;
	}
	private int dinx2cinx(int xid, int dinx) {
	    if (dinx<0) return UNSET;
	    int flop = store.flop(xid);
	    int nth = store.nth(xid);
	    return dinx*nth+flop;
	}

	// domain state manipulation
	public void setCState(RTRealDomain x, int i) throws Xcept {
	    int xid = x.domainID();
	    if (i>=0) {
	    	if (i>=cct(x)) throw new Xcept(x,
		    "Domain index out of bounds (" + i + ")");
	    	cstate[xid] = i;
	    } else {
		cstate[xid] = UNSET;
	    }
	    dstate[xid] = cinx2dinx(xid, cstate[xid]);
	    audit("setCState", x);
	}
	public void setDState(RTRealDomain x, int i) throws Xcept {
	    int xid = x.domainID();
	    if (i>=0) {
	    	GridData gdata = store.gdata(x);
	    	if (gdata == null) throw new Xcept(x, 
		    "Domain setState() with null grid");
	    	if (i>=dct(x)) throw new Xcept(x,
		    "Domain index out of bounds (" + i + ")");
	    	dstate[xid] = i;
	    } else {
		dstate[xid] = UNSET;
	    }
	    cstate[xid] = dinx2cinx(xid, dstate[xid]);
	    audit("setDState", x);
	}
	public void setCInx(RTRealDomain x, int i) throws Xcept {
	    if (i<0) throw new Xcept(x,
		"Negative domain index is invalid");
	    setCState(x, i);
	}
	public void setDInx(RTRealDomain x, int i) throws Xcept {
	    if (i<0) throw new Xcept(x,
		"Negative domain index is invalid");
	    setDState(x, i);
	}
	public void unSet(RTRealDomain x) throws Xcept { 
	    int xid = x.domainID();
	    cstate[xid] = dstate[xid] = UNSET;
	    audit("unSet", x);
	}
	public void setLHBC(RTRealDomain x) throws Xcept { 
	    setCState(x, 0); 
	}
	public void setRHBC(RTRealDomain x) throws Xcept { 
	    setCInx(x, cct(x)-1);
	}
	public void moveRight(RTRealDomain x) throws Xcept { 
	    // what if unSet?,  special case?
	    int xid = x.domainID();
	    int cct = store.cct(x);
	    cstate[xid]++;
	    if (cstate[xid] >= store.cct(x))
		cstate[xid] = UNSET;
	    dstate[xid] = cinx2dinx(xid, cstate[xid]);
	    audit("moveRight", x);
	}
	public void moveLeft(RTRealDomain x) throws Xcept { 
	    // what if unSet?,  special case?
	    int xid = x.domainID();
	    if (cstate[xid]>0) 
		cstate[xid]--;
	    else 
		cstate[xid] = UNSET;
	    dstate[xid] = cinx2dinx(xid, cstate[xid]);
	    audit("moveLeft", x);
	}

	// audit state
	private void audit(String func, RTRealDomain x) throws Xcept {
	    // int xid = x.domainID();
	    ///Util.verbose(func + ": " + x /* +"=" + realVal(x) */ 
	    //    + " cstate=" + cstate[xid] + " dstate=" + dstate[xid]);
	}

	// ndata alloc/free
	public void varAlloc(RTNVar[] vs) throws Xcept {
	    // nothing yet
	}
	public void varFree(RTNVar[] vs) throws Xcept {
	    // nothing yet
	}

	// ndata query
	public int dinx(RTRealNVar v) throws Xcept {
	    switch (v.ndim()) {
	    case 0: 
		return 0;
	    case 1: 
		return dinx(v.doms[0]);
	    default:
		int pos[] = new int[v.ndim()];
		int ct[] = new int[pos.length];
		for (int i=0; i<pos.length; i++) {
		    pos[i] = dinx(v.doms[i]);
		    ct[i] = dct(v.doms[i]);
		}
		return Data.inx(pos, ct);
	    }    
	}
	public RealNData ndata(RTRealNVar v) throws Xcept {
	    RealNData ndata = store.ndata(v);
	    if (ndata == null) {
		ndata = v.makeData(this);
		store.setData(v, ndata);
	    }
	    return ndata;
	}
	public double realVal(RTRealNVar v) throws Xcept {
	    calcAssign(v);
	    RealNData ndata = store.ndata(v);
	    double val = ndata.realVal(dinx(v));
	    if (v instanceof RTIntNVar) val = Math.floor(val);
	    return val;
	}	    
	public double realVal(RTRealNVar v, double[] vals) 
	throws Xcept {
	    calcAssign(v);
	    RealNData ndata = store.ndata(v);
	    double val = ndata.realVal(vals);
	    if (v instanceof RTIntNVar) val = Math.floor(val);
	    return val;
	}	    
	public int intVal(RTRealNVar v) throws Xcept {
	    return (int) realVal(v);
	}

	// calculate input data assignment
	protected void calcAssign(RTVar v) throws Xcept {
	    store.calcAssign(this, v);
	}
 
	// set NData
	public void set(RTRealNVar v, double val) throws Xcept {
	    RealNData ndata = store.ndata(v);
	    if (ndata == null) {
		ndata = v.makeData(this);
		store.setData(v, ndata);
	    }
	    if (v instanceof RTIntNVar) val = Math.floor(val);
	    ndata.set(dinx(v), val);
	    // Util.verbose("set " + v + "[" + dinx(v) + "]=" + val);
	}

	// set NData at t.min
	public void setMin(RTRealNVar v, RTRealDomain x, double val) throws Xcept {
	    int saveState = getCState(x);
	    setLHBC(x);
	    set(v, val);
	    setCState(x, saveState);
	}

	// set NData to last t value
	public double setFromLeft(RTRealNVar v, RTRealDomain x) 
	throws Xcept {
	    if (atLHBC(x) || ! isSet(x)) throw new Xcept(
	    	"RTContext.setFromLeft(" + v + 
		"): domain index " + x + " not set");
	    moveLeft(x);
	    double val = realVal(v);
	    moveRight(x);
	    set(v, val);
	    return val;
	}

	// push curr value forward to next higher x value
	public double setNextRight(RTRealNVar v, RTRealDomain x) 
	throws Xcept {
	    if (atRHBC(x) || ! isSet(x)) throw new Xcept(
	    	"RTContext.setNextRight(" + v + 
		"): domain index " + x + " not set");
	    double val = realVal(v);
	    moveRight(x);
	    set(v, val);
	    moveLeft(x);
	    return val;
	}

	// phase control
	public void startPhase(int i, RTRealDomain x) {
	    store.startPhase(i, x);
	}
	public void updatePhase(double frac) {
	    store.updatePhase(frac);
	}
	public void updatePhase(RTRealDomain x) throws Xcept {
	    double frac = (cinx(x)+1.0) / cct(x);
	    store.updatePhase(frac);
	}

	//// split block solving
	// single thread for now
	public void solve(RTProblem[] probs) throws Xcept {
	    for (int i=0; i<probs.length; i++) 
	    	probs[i].solve(this);
  	}

	//// Version 1.6 tracing
	public static final int TRACE = 1;
	public static final int TRACENAN = 2;
	public static final int ABORTNAN = 4;

	// trace cache value 
 	public final void trace(int action, RTRealNVar v, double val) 
	throws Xcept {
	    if (doAbort(action, val))
	    	throw new Xcept(traceCacheMsg(v, val));
	    if (doTrace(action, val))
	    	System.err.println(traceCacheMsg(v, val));
	}
	private final String traceCacheMsg(RTRealNVar v, double val) {
	    return v.name + "()=" + val;
	}

	// set array value with trace
	public final void setTrace(int action, RTRealNVar v, double val) 
	throws Xcept {
	    set(v, val);
	    if (doAbort(action, val))
	    	throw new Xcept(traceArrayMsg(v, val));
	    if (doTrace(action, val))
	    	System.err.println(traceArrayMsg(v, val));
	}
	private final String traceArrayMsg(RTRealNVar v, double val) 
	throws Xcept {
	    return v.name + "[" + dinx(v) + "]=" + val;
	}

	// do a trace
	private final boolean doAbort(int action, double val) {
	    return (action & ABORTNAN)>0 && Double.isNaN(val);
	}
	private final boolean doTrace(int action, double val) {
	    if ((action & TRACE)>0) return true;
	    if ((action & TRACENAN)>0 && Double.isNaN(val)) return true;
	    return false;
	}

	//// Version 2.0 tracing
	public boolean abortNaN() { return false; } // override if needed
	public void trace(RTVar v, double val, String mudoms) 
	throws Xcept {
	    int dinx = (v instanceof RTRealDomain) ?
	    	dinx((RTRealDomain) v) : dinx((RTRealNVar) v);
	    String msg = v.name + "[" + dinx;
	    if (mudoms != null) msg = msg + "; " + mudoms;
	    msg = msg + "] = " + Util.pretty(val);
	    if (abortNaN()) throw new Xcept(msg);
	    System.err.println(msg);
	}


}
