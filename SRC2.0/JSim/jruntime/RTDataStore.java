/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// store for current or previous model run

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 
import java.util.ArrayList;

public final class RTDataStore implements NamedVal.Query {
	private RTModel model;	// for this model
	private String name; 	// name for this store, for loops
	private String sensP;	// for this sensitivity par (if any)
	private double sensDelta; // delta for sensP
	private Data[] data;	// data items
	private boolean assignPending[]; // calcAssign needed?
	private NamedVal.NList overrides; // model overrides
	private boolean isRunning; // this store currently running?
	private RTContext runCtxt; // top-level run context

	private RTLiveQuery.List liveQueries; // live queries
	private ASInfo.RunStatus runStat; // curr run status
	private RTRealDomain runDomain; // if in mainline domain loop
	protected boolean skipRun; // terminate run if set

	// distict comp/data grids
	private GridData[] cgrids; // comp grids by domain
	private int[] nths; // every nth, count by domain
	private int[] flops; // # left excess cgrid points

	// new run constructor
	public RTDataStore(RTModel m) {
	    model = m;
	    sensDelta = Double.NaN;
	    int nx = model.domains.size();
	    cgrids = new GridData[nx];
	    nths = new int[nx];
	    flops = new int[nx];
	    for (int i=0; i<nx; i++)
	    	nths[i] = flops[i] = -1;
	    clear();
	}
	
	// clear a previous data in store (if any)
	public void clear() {	    
	    data = new Data[model.vars.size()];
	    assignPending = new boolean[model.vars.size()];
	    for (int i=0; i<assignPending.length; i++) 
		if (model.vars.var(i).isInput()) 
		    assignPending[i] = true;
	    overrides = null;
	    liveQueries = new RTLiveQuery.List();
	}

	// set properties
	protected void setName(String n) { 
	    name = n; 
	}
	protected void setSens(String p, double delta) {
	    sensP = p;
	    sensDelta = delta;
	}
	protected void setData(RTVar v, Data d) {
	    d.setGroup(name);
	    data[v.varID()] = d;
	}
	protected void setDomainData(RTRealDomain x,
	GridData dgrid, GridData cgrid, int nth, int flop) throws Xcept {
	    setData(x, dgrid);
	    int xid = x.domainID();
	    cgrids[xid] = cgrid;
	    nths[xid] = nth;
	    flops[xid] = flop;
	}

	// create new top context for run in this store
	protected void runPrep(RTContext ctxt, NamedVal.NList overs) throws Xcept {
	    isRunning = true;
	    clear();
	    overrides = overs;
	    runCtxt = ctxt;
	    runStat = new ASInfo.RunStatus();
	    runDomain = null;
	}	
	    
	// get live data, entire or partial
	protected Data getLiveData(RTContext ctxt, Expr expr) throws Xcept {
	    RTLiveQuery query = liveQueries.query(expr);
	    if (query == null) {
		query = new RTLiveQuery(model, expr);
		liveQueries.add(query);
	    }
	    if (runStat.phase < query.phase())
		return null; // no data yet
	    if (runStat.phase > query.phase()) 
		return query.getAllData(ctxt); // complete data
	    if (runDomain == null)
		return null; // no domain, must wait till next phase

	    // get partial data during live phase
	    if (runCtxt == null) return null;
	    int hix = runCtxt.dinx(runDomain);
	    query.update(ctxt, runDomain, hix);
	    Data data = query.data();
	    return data;
	}

	// start new phase of calculation
  	protected void startPhase(int i, RTRealDomain x) {
	    runStat.phase = i;
	    runStat.frac = 0;
	    runDomain = x;
	}

	// loop progress within phase
  	protected void updatePhase(double frac) {
	    runStat.frac = frac;
	}

	protected void runCompleted() {
	    isRunning = false;
	    // clear live queries, other?
	}

	// calc assign, if needed
  	protected void calcAssign(RTContext ctxt, RTVar v) throws Xcept {
	    int vid = v.varID();
	    if (! assignPending[vid]) return;
	    if (! (v instanceof RTRealNVar)) throw new Xcept(v,
		"Cannot calculated assignment for variable");

	    // assign has store-specific override?
	    Expr expr = null;
	    if (overrides != null) {
	    	NamedVal nval = overrides.namedVal(v.name());
		if (nval != null)
		    expr = Expr.cons(nval.realVal());
	    }
	    
	    // do assign
	    RealNData ndata = 
	    	((RTRealNVar) v).calcAssign(this, expr, ctxt.random);
	    setData(v, ndata);
	    assignPending[vid] = false; 
	}

	// query name & data
	public String name() { return name; }
	public String sensP() { return sensP; }
	public double sensDelta() { return sensDelta; }
	public ASInfo.RunStatus runStat() { return runStat; }
	public Data data(RTVar v) {
	    return data[v.varID()];
	}
	public RealNData ndata(RTVar v) {
	    return (RealNData) data[v.varID()];
	}
	public GridData gdata(RTVar v) {
	    return (GridData) data[v.varID()];
	}
	public boolean isRunning() { return isRunning; }
	public RTContext runCtxt() { return runCtxt; }

	// comp/data grid queries
	public GridData cgrid(RTRealDomain x) {
	    return cgrids[x.domainID()];
	}
	public int cct(RTRealDomain x) {
	    return cgrids[x.domainID()].ct();
	}
	public int nth(int xid) {
	    return nths[xid];
	}
	public int nth(RTRealDomain x) {
	    return nths[x.domainID()];
	}
	public int flop(int xid) { 
	    return flops[xid];
	}


	// query local namedVals first,  then model's
 	public NamedVal namedVal(String n) throws Xcept {
	    if (overrides != null) { 
	    	NamedVal nv = overrides.namedVal(n);
	    	if (nv != null) return nv;
	    }
	    return model.namedVal(n);
	}

	// RTDataStore.List
	public static class List extends ArrayList<RTDataStore> {
	    public List() { super(); }
	    public RTDataStore store(int i) throws Xcept {
		if (i<0 || i>=size()) throw new Xcept(
		    "invalid RTDataStore index"); 
		return (RTDataStore) get(i);
	    }
	}
}
