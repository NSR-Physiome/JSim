/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// abstract Run-time model object

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.nml.*;
import JSim.nml.opt.*;
import JSim.nml.pde1.PDE1Solver;

public abstract class RTModel implements DiagInfo, NameSpace {
	public String name;	// model name
	public ASServer.Messenger messenger; // if any
	public RTVar.NList vars;	// all model vars
	protected RTRealDomain.NList domains;  // subset of vars
	public RTProblem.List probs;  // all model problems
	public UnitNList units;	// internally required units for vars
	protected boolean unitCorrect;	// true if unit correction on
	protected ASModel.Flags flags; // unit/solver flags
	private RTDataStore[] stores; // allocated stores
	private RTFuncGen.List funcGens; // function generator list
	private NamedVal.NList runVals; // non-default run values
	public boolean cancelRun; // cancel this run
	private OptimFactory optimFactory; // for fzero solvers

	protected RTMemory memory; // memory scheme for current run
 	protected int maxProc; // max # MP calc threads
	protected int maxThreads; // # threadInx slots for next run

      	// constructor
	public RTModel(String n, UnitNList sysUnits,
	ASServer.Messenger msgr) 
	throws Xcept {
	    name = n;
	    messenger = msgr;
	    vars = new RTVar.NList(16);
	    domains = new RTRealDomain.NList(2);
	    probs = new RTProblem.List(4);
	    units = new UnitNList();
	    if (sysUnits != null) units.merge(sysUnits);
	    unitCorrect = defaultUnitCorrect();
	    funcGens = new RTFuncGen.List();
	    runVals = new NamedVal.NList();
	}

	// call this when done with subclass constructor
	protected void doneConstructor() throws Xcept {
	    clearStores();
	}

	// get/set OptimFactory
	public void setOptimFactory(OptimFactory f) {
	    optimFactory = f;
	}
	public OptimFactory optimFactory() { return optimFactory; }

	// is MML unit corrected?
	public abstract boolean defaultUnitCorrect(); 

	// set model to default assignments and units
	public void setDefault() throws Xcept {
	    for (int i=0; i<vars.size(); i++) 
		vars.var(i).setDefault();
	}

	// add stuff to model
	public void add(RTVar v) throws Xcept { vars.add(v); }
	public void add(RTProblem p) throws Xcept { probs.add(p); }	

	// allow parallel runs of this model?
	public boolean allowMPRuns() { return false; }

	// max # proc threads used to allocate Fzero solvers[]
	//   solvers[threadInx][loopDomain]
	public void setMaxProc(int n) { 
	    maxProc = n;
	    maxThreads =  2*n + 1;  // Mopt is worst case
	} 

	// initialize memory scheme at start of various runs
	public void initMemory(NamedVal.NList runVals) throws Xcept {
	    memory = new RTMemory(this, runVals);
	}

	// create model-specific run context (override in user model)
	public RTContext newRunContext(int threadInx, int nproc, 
	RTDataStore s) throws Xcept {
	    return new RTContext(this, threadInx, nproc, s, false);
	}

	// mainline calculation
	public abstract void mainline(RTContext ctxt) throws Xcept;

	// allocate stores in preparation for run
	//   run garbage collector explicity for more eff. alloc?
	public void allocStores(int n) throws Xcept {
	    stores = new RTDataStore[n];
	    Runtime.getRuntime().gc();
	    for (int i=0; i<n; i++) 
	    	stores[i] = new RTDataStore(this);
	    for (int i=0; i<probs.size(); i++) 
	    	probs.prob(i).allocThreads(maxThreads);
	    for (int i=0; i<funcGens.size(); i++) 
	    	funcGens.get(i).allocThreads(maxThreads);	    
	}

	// allocate stores for single, loops, optim
	public void allocStores(String[] names) throws Xcept {
	    allocStores(names.length);
	    for (int i=0; i<names.length; i++) 	    
		stores[i].setName(names[i]);
	}

	// allocate stores for sensitivity
	public void allocStores(String[] sensP, double[] sensDelta)
	throws Xcept {
	    allocStores(sensP.length+1);
	    for (int i=0; i<sensP.length; i++) 
		stores[i+1].setSens(sensP[i], sensDelta[i]);
	}

	// clear stores
	public void clearStores() throws Xcept {
	    stores = new RTDataStore[0];
	    Runtime.getRuntime().gc();
	}

	// run model in given storeInx
	public void run(int threadInx, int nproc, int storeInx, NamedVal.NList overrides) throws Xcept  {
	    // System.err.println("run threadInx=" + threadInx + " " + overrides);
	    // threadInx sanity check
	    if (threadInx >= maxThreads) throw new Xcept(
	   	"Model run threadInx=" + threadInx +
		" exceeds maxThreads=" + maxThreads);

	    // prepare then start captive run
	    RTDataStore store = stores[storeInx];
	    RTContext ctxt = newRunContext(threadInx, nproc, store);
	    store.runPrep(ctxt, overrides);
	    int seed = store.namedVal("solver.random_seed").intVal();
	    ctxt.setRandomSeed(seed);
	    Xcept x = null;
	    try {
	    	runCaptive(ctxt);
		store.runCompleted();
	    } catch (Exception e) {
	        store.runCompleted();
		if (! store.skipRun)
		    throw Xcept.wrap(e);
	    }
	}

	// run model
	public void runCaptive(RTContext ctxt) throws Xcept {
	    ctxt.startPhase(1, null);

	    // check all inputs assigned
	    // build lists of static & dynamic input vars for sequencing
	    RTVar.List stat = new RTVar.List(16);
	    RTVar.List dyn = new RTVar.List(16);
	    for (int i=0; i<vars.size(); i++) {
		RTVar v = vars.var(i);
		if (! v.isInput()) continue;
		if (v.assign == null) {
		    throw new Xcept(v,
		     "Variable has no valid assigned value");
		}
		if (v.ndim() > 0) dyn.add(v); else stat.add(v);
	    }

	    // initialize problem solvers for run
	    for (int i=0; i<probs.size(); i++) 
	    	probs.prob(i).runPrep(ctxt);

	    // initialize funcgens for run
	    for (int i=0; i<funcGens.size(); i++) 
	    	funcGens.get(i).runPrep(ctxt);

	    // sequence static inputs
	    RTVar.List known = new RTVar.List(16);
	    stat = sequence(stat, known);

	    // sequence dynamic inputs
	    for (int i=0; i<vars.size(); i++) {
		RTVar v = vars.var(i);
		if (v.isDomain() || stat.contains(v))
		    known.add(v);
	    }
	    dyn = sequence(dyn, known);

	    // set static input vars to assigned values
	    for (int i=0; i<stat.size(); i++) 
		ctxt.calcAssign(stat.var(i));

	    // model mainline
	    ctxt.startPhase(2, null);
	    mainline(ctxt);

	    // make sure all dyn inputs are calculated
	    for (int i=0; i<dyn.size(); i++) 
		ctxt.calcAssign(dyn.var(i));
	}

	// copy store (for optimizer)
	public void copyStore(int fromInx, int toInx) throws Xcept {
	    stores[toInx] = stores[fromInx];
	    stores[fromInx] = new RTDataStore(this);
	}

	// sequence varlist
	public RTVar.List sequence(RTVar.List vlist, RTVar.List known)
	throws Xcept {
	    Util.verbose("==== Ordering input assignments");
	    RTVar.List olist = new RTVar.List(vlist.size() + 1);
	    boolean working = true;
	    while (working) {
		working = false;
		for (int i=0; i<vlist.size(); i++) {
		    RTVar v = vlist.var(i);
		    if (olist.contains(v)) continue;
		    RTVar.List xlist = new RTVar.List(1);
		    v.assign.addNamedExpr(xlist);
		    boolean ok = true;
		    for (int j=0; j<xlist.size(); j++) {
			if (! RTVar.class.isInstance(xlist.get(j)))
			    continue;
			RTVar x = xlist.var(j);
			if (! x.isInput() && ! x.isDomain()) throw new Xcept(
			    "Input variable " + v + 
			    " may not be calculated from output variable " + x);
			if (known.contains(x)) continue;
			if (olist.contains(x)) continue;
			ok = false;
			break;
		    }
		    if (ok) {
			Util.verbose("\t" + v + " = " + v.assign);
			olist.add(v);
			working = true;
		    }
		}
	    }
	    if (! olist.containSet(vlist)) throw new Xcept(
		"Input variable assignments have circular dependencies");
	    return olist;
	}

	// set skip flag
	public void setSkipRun(int storeInx) {
	    if (stores == null) return;
	    if (stores.length <= storeInx) return;
	    RTDataStore store = stores[storeInx];
	    if (store == null) return;
	    store.skipRun = true;
	}

	// simple query
	public String toString() { 
	    return "Model " + this.getClass().getName();
	}
	final public String diagInfo() { return toString(); }	
	final public ASInfo.RunStatus getRunStat(int inx) { 
	    if (inx >= nstores()) return null;
	    if (stores[inx] == null) return null; // in case query too fast
	    ASInfo.RunStatus runStat = stores[inx].runStat();
	    if (runStat == null) return null;
	    return new ASInfo.RunStatus(runStat); 
	}

	// query last store (for RTVar.finalRealVal)
	//   arguably wrong for loops, but need addn info for that
	final protected RTDataStore finalStore() throws Xcept {
	    if (nstores() < 1) return null;
	    return stores[0]; 
	}

	// return public vars 
	public RTVar.List getVars() {
	    RTVar.List list = new RTVar.List(1);
	    for (int i=0; i<vars.size(); i++) {
		RTVar v = vars.var(i);
		if (v.isPrivate()) continue;
		list.add(v);
	    }
	    return list;
	}
	public RTVar getVar(String n) throws Xcept {
	    RTVar v = (RTVar) vars.getByName(n);
	    if (v == null) throw new Xcept("Variable " + n + " unknown");
	    if (v.isPrivate()) throw new Xcept("Variable " + n + " is private.");
	    return v;
	}	
	
	// namedVal query
	public NamedVal namedVal(String s) throws Xcept {
	    if (runVals != null) {
	    	NamedVal nval = runVals.namedVal(s);
		if (nval != null) return nval;
	    }
	    throw new Xcept("No RTModel.namedVal for " + s);
	}

	// namespace query
	public Expr compByName(String s) throws Xcept {
	    RTVar v = (RTVar) vars.getByName(s);
	    if (v != null) {
	    	if (v.isPrivate()) throw new Xcept("Variable " + s + " is private.");
	    	return v;
	    }
	    NamedExpr sv = null;
	    sv = RTSolverSettings.getSettingVar(this, s);
	    if (sv != null) return sv;
	    for (int i=0; i<funcGens.size(); i++) {
	    	sv = funcGens.get(i).getVar(s);
		if (sv != null) return sv;
	    }
	    throw new Xcept("Unknown variable: " + s);
	}
	public Unit unitByName(String s) throws Xcept {
	    return units.byName(s);
	}
	public RTFuncGen.List funcGens() { return funcGens; }

	// function Call from name & args
	public Expr funcCall(String n, Expr.List elist) throws Xcept {
	    RTVar v;
	    try {
		v = getVar(n);
	    } catch (Xcept e) {
		return IExpr.create(n, elist);
	    }
	    if (v.isDomain() || elist.size() != v.ndim()) 
		throw new Xcept(v, "variable function requires " + 
		    v.ndim() + " arguments");
	    Expr.List domlist = v.domainList();
	    if (domlist.sameAs(elist))
		return v;
	    return new RTVarFuncCall((RTRealNVar) v, elist);
	}

	// parse String creating Expr
	public Expr parseExpr(String s) throws Xcept {
	    Expr expr = Expr.parse(this, s);
	    if (unitCorrect) 
		expr.unitCorrect(); // check unit consistency
	    return expr;  // but return uncorrected version
	}

	// unit for Expr
	public Unit unit(Expr expr) throws Xcept {
	    if (unitCorrect && IExpr.class.isInstance(expr)) {
		expr = expr.unitCorrect();
	    }
	    Unit u = expr.unit();
	    return (u==null) ? Unit.scalar() : u;
	}

	// parse String creating Unit
	public Unit parseUnit(String s) throws Xcept {
	    return Unit.parse(this, "1 " + s);
	}

	// make deriv expression
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    return RTDeriv.create(this, e1, e2);
	}

	// external data query
	public Data getData(int n, Expr expr) throws Xcept {
	    if (n >= nstores()) return null;
	    RTDataStore store = stores[n];
	    if (store == null) return null; // out-of-sequence query
	    RTContext ctxt = store.runCtxt();

	    // below in case of asynchronous getData between
	    //   RTDataStore constructor and store.runPrep()
	    // possible, but highly improbable
	    if (ctxt == null) return null; 

	    int tx = ctxt.threadInx;
	    RTContext qctxt = new RTContext(this, tx, 1, stores[n], true);
	    Data data = getData(qctxt, expr, null, null);
	    if (store.storeNth != null) 
	        data = store.storeNth.getData(ctxt, data);
	    return data;
	}

	// get internal data from appropriate RTDataStore
	protected Data getData(RTContext ctxt, Expr expr, 
	RTRealDomain[] addDoms, Unit forceUnit) throws Xcept {
	    try {
		if (ctxt.isQuery() && ctxt.store.isRunning()) 
 		    return ctxt.store.getLiveData(ctxt, expr);
 		else {
		    Data d = getAllData(ctxt, expr, addDoms, forceUnit);
		    if (d != null) d.subset = null;
		    return d;
		}		
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}
		
	// get entire data set
	protected Data getAllData(RTContext ctxt, Expr expr, 
	RTRealDomain[] addDoms, Unit forceUnit) throws Xcept {
	    boolean nomake = (forceUnit == null) && 
		(expr instanceof RTVar);
	    if (nomake) 
		return ctxt.store().data((RTVar) expr);
	    else
		return makeData(ctxt, expr, addDoms, forceUnit);
	}

	// make expr data
	private RealNData makeData(RTContext ctxt, Expr expr, 
	RTRealDomain[] addDoms, Unit forceUnit) throws Xcept {
	    // create blank RealNData
	    RTRealDomain.List doms = new RTRealDomain.List(4);
	    if (addDoms != null) 
		for (int i=0; i<addDoms.length; i++) 
		    doms.addUniq(addDoms[i]);
	    expr.addDomains(doms);
	    GridData[] grids = new GridData[doms.size()];
	    for (int i=0; i<doms.size(); i++) {
		RTRealDomain x = (RTRealDomain) doms.get(i);
		grids[i] = ctxt.grid(x); // coarse or fine ???
	    }
	    String desc = expr.toString();

	    // fill in samples
	    if (unitCorrect) expr = expr.unitCorrect();
	    Unit exprUnit = expr.unit();
	    Unit dataUnit = (forceUnit == null) ? exprUnit : forceUnit;
	    RealNData ndata = new RealNData(desc, dataUnit, grids);
	    ndata.setGroup(ctxt.store().name());
	    double ufactor = 1;
	    if (unitCorrect) {
		if (! Unit.compatible(expr.unit(), dataUnit)) throw new Xcept(
		    expr, "assignment incompatible with unit " + dataUnit.pubName());
		ufactor = Unit.convertFactor(dataUnit, expr.unit());
	    }
	    int nsamp = ndata.nsamples();
	    for (int i=0; i<nsamp; i++) {
		int[] gpos = ndata.gridPos(i);
		for (int j=0; j<doms.size(); j++) {
		    RTRealDomain x = (RTRealDomain) doms.get(j);
		    ctxt.setDInx(x, gpos[j]);
		}
		ndata.set(i, expr.realVal(ctxt) * ufactor);
	    }	

	    // done, return
	    return ndata;
	}
	
	// get store name 
	public String getStoreName(int i) {
	    if (i<0 || i>=nstores()) return null;
	    return stores[i].name();
	}

	// bad relation
	public void badRelation(String s) throws Xcept {
	    throw new Xcept(this, "Required relation failed: " + s);
	}

	// inconsistent IC/BC
	public void badICBC(String s, NamedExpr v, double resid) throws Xcept {
	    throw new Xcept(this, s + ": residual=" + resid);
	}

	// get sensitivity store for parameter p
	protected RTDataStore sensStore(String p) throws Xcept {
	    for (int i=1; i<stores.length; i++) {
		String sp = stores[i].sensP();
		if (sp != null && p.equals(sp)) 
		    return stores[i];
	    }
	    return null;
	}

	// # current stores
	public int nstores() { 
	    if (stores == null) return 0;
	    return stores.length; 
	}

	// model-generated warning to user
  	public void warning(String msg) {
	    if (messenger != null)
	    	messenger.message(
		    new ASInfo.Message(null, true, msg));
	}

	// set function generators
	public void setFuncGenNames(String[] names) throws Xcept {
	    boolean changed = funcGens.update(this, names);
	    if (changed) ; // ??? reparse Fgen dependent Queries
	}

	// set runVals, null protect
	public void setRunVals(NamedVal.NList nvals) {
	    runVals = (nvals == null) ?
	    	new NamedVal.NList() : nvals;
	}

	// get flags
	public ASModel.Flags getFlags() {
	    if (flags != null) return flags;
	    flags = new ASModel.Flags();
	
	    // unit flags
	    flags.needsUnitCorrect = defaultUnitCorrect();
	    for (int i=0; i<vars.size(); i++) {
		Unit u = vars.var(i).unit();
		if (Unit.same(u, Unit.scalar())) continue;
		flags.needsUnits = true;
		break;
	    }

	    // solver flags
	    for (int i=0; i<probs.size(); i++) {
	    	RTProblem prob = probs.prob(i);
		if (prob instanceof ODE1Problem)
		    flags.usesODESolver = true;
		if (prob instanceof PDE1Problem) {
		    PDE1Problem pprob = (PDE1Problem) prob;
		    for (int j=0; j<ASModel.PDE_Solvers.length; j++) 
		    	flags.usesPDESolvers[j] = 
		            pprob.usesPDESolver(j);
		}
		if (prob instanceof Fzero2Problem)
		    flags.usesFzero2Solver = true;
	    }

	    // random number generator
	    flags.usesRandom = true; // can't tell yet
	    return flags;
	}

	// clear profiling info
	public void clearProfile() {
	    for (int i=0; i<probs.size(); i++) 
	    	probs.prob(i).clearProfile();
	}
	
	// collect profiling info
	public ProfileData getProfile() {
	    ProfileData prof = new ProfileData();
	    prof.desc = "not available";
	    prof.problems = new ProfileData.Problem[probs.size()];
	    for (int i=0; i<probs.size(); i++) 
	    	prof.problems[i] = probs.prob(i).getProfile();
	    return prof;
	}

	// can store only every Nth point at runtime 
	//   without affecting results? 
  	public boolean runTimeNthSupported(RTRealDomain x) {
	    return true; // Turn off runtime Nth support as needed
	}
}  

