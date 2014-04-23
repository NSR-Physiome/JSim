/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ongoing connection to remote model run-time

package JSim.rclient;

import java.io.*;
import java.rmi.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 

public class RCModel implements DiagInfo, ASModel {

	// base state
	private RCClient client;	// app-side client
	private String id;
	private boolean isBuilt;	// is model built?
	private RCInfo.Model info;	// initialization info
	
	// compile state
	private UnitNList units;	// model units
	private ASVar.NList nasvars;	// named local asvars
	private ASVar.List asvars;	// flat local asvars HACK!!!
	private RCDataStore[] stores; // data store caches
	private int storeMode; // see ASModel constants
	private OptimResults optimResults; // latest optim results
	private MoptData moptData; // latest Mopt results
	private ASInfo.JobInfo jobInfo; // job info
	private ASInfo.Status jobStat; // job status as of last poll
	private int nrunsLast; // #runs completed as of last boll
	private String[] buildAlerts; // build errors & warnings
	private String[] modelTextCache; // cached getText queries
	private String[][] modelTextWarningCache; // cached getText warnings

	// constructor
	public RCModel(RCClient c) throws Xcept {
	    client = c;
	    unbuildRT();
	}	    

 	// unique model identifier within ASServer
 	public String modelID() { return "rcmodel" + hashCode(); }

	// unbuild the model
	public void unbuildRT() {
	    id = client.nextModelID();
	    isBuilt = false;
	    info = null;
	    units = null;
	    nasvars = null;
	    asvars = null;
	    storeMode = SINGLE;
	    allocStores(0);
	    optimResults = null;
	    moptData = null;
	    modelTextCache = null;
	    modelTextWarningCache = null;
	    buildAlerts = null;
	}

	// allocate stores
	private void allocStores(int n) { allocStores(new String[n]); }
	private void allocStores(String[] names) {
	    stores = new RCDataStore[names.length];
	    for (int i=0; i<names.length; i++)
	        stores[i] = new RCDataStore(this, i, names[i]);
	}

	// build the model
	public void buildRT(ASInfo.Build build) throws Xcept {
	    unbuildRT();
	    ASInfo.BuildInfo binfo = new ASInfo.BuildInfo(build);
	    modelTextCache = new String[ASModel.TEXT_NAMES.length];
	    modelTextWarningCache = new String[ASModel.TEXT_NAMES.length][];
	    jobInfo = build;
	    
	    // start server-side build
	    try {
		request();
	    	RCInfo.JobStatus jstat = 
		    rclient().startBuild(id, binfo);
		client.waitForJob(null, jstat);
		request();
		info = (RCInfo.Model) rclient().getJobOutput(jstat.id);
	    	buildAlerts = rclient().getBuildAlerts(id);
	    } catch (RemoteException e) {
		throw new Xcept(e.getMessage());
	    }
	    isBuilt = true;

	    // initialize units
	    if (info.units != null)
		units = info.units.unitList();

	    // initialize vars
	    nasvars = new ASVar.NList(info.vars.length);
	    asvars = new ASVar.List(info.vars.length);
	    for (int i=0; i<info.vars.length; i++) {
		ASVar v = new RCModelVar(this, info.vars[i]);
		nasvars.add(v);
		asvars.add(v);
	    }
	    for (int i=0; i<asvars.size(); i++) {
		RCModelVar v = (RCModelVar) asvars.asvar(i);
		v.setDomains();
	    }

	}

	// get build errors & warnings
	public String[] getBuildAlerts() {
	    return buildAlerts;
	}

	// check build state
	protected void checkBuilt(String s) throws Xcept {
	    if (isBuilt) return;
//	    Thread.currentThread().dumpStack();
	    throw new Xcept(this,
	    	s + "() called while model not compiled");
	}

	// rclient query
	public RCClient client() { return client; }
	public RCClientIF rclient() { return client.rclient(); }
	public String id() { return id; }
	public String diagInfo() { return "RCModel"; }
	public RCInfo.Model getInfo() { return info; }

	// public ASModel query
	public boolean isBuilt() { return isBuilt; }
	public ASVar.List getASVars() throws Xcept { 
	    checkBuilt("getASVars");
	    return asvars; 
	}
	public ASVar getASVar(String n) throws Xcept { 
	    checkBuilt("getASVar");
	    ASVar v = nasvars.asvar(n); 
	    if (v == null) throw new Xcept(this,
		"Variable " + n + " unknown");
	    return v;
	}
	public UnitNList units() throws Xcept { 
	    checkBuilt("units");
	    return units; 
	}
	public int nstores() { 
	    if (! isBuilt) return 0;
	    if (stores == null) return 0; // needed?
	    return stores.length; 
	}
	
	// get diag text
	public String getText(int type, String variant) {
	    try {
		// check modelTextCache first
		boolean useCache = useCache(type, variant);
		if (useCache && modelTextCache[type] != null)
		    return modelTextCache[type];
		
		// make remote request
		request();
	    	String text = rclient().getText(id(), type, variant);
		if (useCache) {
		    modelTextCache[type] = text;
		    modelTextWarningCache[type] = 
		    	rclient().getTextWarnings(id(), type, variant);
		}
		return text;
	    } catch (RemoteException e) {
		return e.toString();
	    }
	}

	// get text warnings
	public String[] getTextWarnings(int type, String variant) {
	    boolean useCache = useCache(type, variant);
	    return useCache ? modelTextWarningCache[type] : null;
	}

	// use text/warning caches?
	private boolean useCache(int type, String variant)  {
	    if (modelTextCache == null) return false;
	    if (type < 0) return false;
	    if (type >= modelTextCache.length) return false;
	    if (! Util.isBlank(variant)) return false;
	    return true;
	}

	// set assignment
	protected void setAssign(RCModelVar v, String s) throws Xcept {
	    if (Util.isBlank(s)) throw new Xcept(
	    	"Variable " + v.name() + " assignment missing");
	    double rval = Util.toDouble(s);
	    if (! Double.isNaN(rval)) return;
	    try {
	    	rclient().setAssign(id(), v.name(), s);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// single model run
	public void singleRun(NamedVal.NList runVals) throws Xcept {
	    checkBuilt("singleRun");
	    storeMode = SINGLE;
	    jobInfo = null;
	    allocStores(1); 
	    RCInfo.RunInput rinfo = 
	    	new RCInfo.RunInput(this, runVals);
	    try {
		request();
	    	RCInfo.JobStatus jinfo = 
		    rclient().startSingleRun(id(), rinfo);
		client.waitForJob(this, jinfo);
		request();
		updateRunOutput(jinfo.id);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

  	// run model loops
	public void loopsRun(ASInfo.Loops loops) throws Xcept {
	    checkBuilt("loopsRun");
	    storeMode = LOOPS;
	    jobInfo = loops;
	    allocStores(loops.runNames); 
	    RCInfo.LoopsInput rinfo = 
	        new RCInfo.LoopsInput(this, loops);
	    try {
		request();
	    	RCInfo.JobStatus jinfo = 
		    rclient().startLoopsRun(id(), rinfo);
		client.waitForJob(this, jinfo);
		request();
		updateRunOutput(jinfo.id);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}
	
	// run sensitivity analysis
	public void sensRun(ASInfo.Sens sens) throws Xcept {
	    checkBuilt("sensRun");
	    storeMode = SENS;
	    jobInfo = sens;
	    allocStores(sens.parNames.length+1);
	    RCInfo.SensInput rinfo = 
	        new RCInfo.SensInput(this, sens);
	    try {
		request();
	    	RCInfo.JobStatus jinfo = 
		    rclient().startSensRun(id(), rinfo);
		client.waitForJob(this, jinfo);
		request();
		updateRunOutput(jinfo.id);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// optimization run
	public void optimRun(ASInfo.Optim optim) throws Xcept {
	    checkBuilt("optimRun");
	    storeMode = OPTIM;
	    jobInfo = optim;
	    allocStores(2); 
	    optimResults = new OptimResults(optim.args);
	    RCInfo.OptimInput rinfo = 
	        new RCInfo.OptimInput(this, optim);
	    try {
		request();
	    	RCInfo.JobStatus jinfo = 
		    rclient().startOptimRun(id(), rinfo);
		client.waitForJob(this, jinfo);
		request();
		updateOptimOutput(jinfo.id);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// mult-optim run
	public void moptRun(ASInfo.Mopt mopt) throws Xcept {
	    checkBuilt("moptRun");
	    storeMode = MOPT;
	    jobInfo = mopt;
	    allocStores(3); // correct ???
	    moptData = null;
	    RCInfo.MoptInput rinfo = 
	        new RCInfo.MoptInput(this, mopt);
	    try {
		request();
	    	RCInfo.JobStatus jinfo = 
		    rclient().startMoptRun(id(), rinfo);
		client.waitForJob(this, jinfo);
		request();
		updateMoptOutput(jinfo.id);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}

	// get optimOutput and update
	private void updateOptimOutput(String jobID) throws Exception {
	    updateOptimOutput(rclient().getJobOutput(jobID));
	}
	protected void updateOptimOutput(Serializable s) throws Xcept {
	    RCInfo.OptimOutput oo = (RCInfo.OptimOutput) s;
	    if (oo == null) return;
	    if (oo.orinfo != null) 
	    	optimResults.importInfo(oo.orinfo);
	    if (oo.runOutput != null) {
	    	stores[0] = new RCDataStore(this, 0, null);
	    	updateRunOutput(oo.runOutput);
	    }
	}   

	// get moptOutput and update
	private void updateMoptOutput(String jobID) throws Exception {
	    updateMoptOutput(rclient().getJobOutput(jobID));
	}
	protected void updateMoptOutput(Serializable s) throws Xcept {
	    RCInfo.MoptOutput moo = (RCInfo.MoptOutput) s;
	    if (moo == null) return;
	    if (moo.morinfo != null) {
	    	if (moptData == null)
	            moptData = moo.morinfo.mopt();
		else
		    moptData.importInfo(moo.morinfo);
	    }
	    if (moo.runOutput != null) {
	    	stores[0] = new RCDataStore(this, 0, null);
	    	updateRunOutput(moo.runOutput);
	    }
	}   

	// get latest job status
	public ASInfo.Status getJobStatus() {
	    return jobStat;
	}

	// skip next loop
	public void nextLoop() {
	    try {
		request();
	    	rclient().nextLoop();
	    } catch (Exception e) { }
	}

	// cancel job
	public void cancelJob() {
	    try {
		request();
	    	rclient().cancelJob();
	    } catch (Exception e) { }
	}
	
	// update all RunOutputs for a job (single, loops, sens)
	private void updateRunOutput(String jobID) throws Exception {
	    Object o = rclient().getJobOutput(jobID);
	    if (o == null) return;
	    if (o instanceof RCInfo.RunOutput) {
	        updateRunOutput((RCInfo.RunOutput) o);
		return;
 	    }
	    
	    // FUTURE: loops may return series of runOutputs
     	    RCInfo.RunOutput[] runOutputs = (RCInfo.RunOutput[]) o; 
	    for (int i=0; i<runOutputs.length; i++) 
	    	updateRunOutput(runOutputs[i]);
       	}
	    
	// update one RunOutput
	private void updateRunOutput(RCInfo.RunOutput runOutput) 
	throws Xcept {
	    if (runOutput == null) return;
	    int storeInx = runOutput.storeInx;
	    if (storeInx >= nstores()) return;
	    stores[storeInx].setFinalVals(runOutput.finalVals);

	    // update user-visible final value
	    if (runOutput.finalVals == null) return;
	    if (storeInx != 0) return;
	    for (int i=0; i<runOutput.finalVals.length; i++) { 
		RCModelVar v = (RCModelVar) asvars.asvar(i);
		v.setFinalVal(runOutput.finalVals[i]);
	    }
	}

	// update live data caches
	protected void updateCacheDeltas(RCInfo.CacheDelta[] deltas)
	throws Xcept {
	    if (deltas == null) return;
	    for (int i=0; i<deltas.length; i++) {
	        RCInfo.CacheDelta delta = deltas[i];
		if (delta.storeInx >= nstores()) return;
	        stores[delta.storeInx].updateDelta(delta);
	    }
	}

	// update job status
	protected void updateJobStatus(ASInfo.Status js) {
	    ASInfo.Status oldStat = jobStat;
	    jobStat = js;
	    
	    // clear out-of-date stores[0] during SENS and OPTIM runs 
	    if (jobStat == null || oldStat == null) return;
	    boolean clear0 = false;
	    if (jobStat.mode == ASModel.SENS 
	    && jobStat.nrunsDone > oldStat.nrunsDone) 
	        clear0 = true;
	    if (jobStat.mode == ASModel.OPTIM
	    && jobStat.nrunsBest > oldStat.nrunsBest) 
	        clear0 = true;
	    if (clear0)
	    	stores[0] = new RCDataStore(this, 0, null);
	}

	// set func gens
	public void setFuncGenNames(String[] names) throws Xcept {
	    // NO! generates spurious error checkBuilt("setFuncGenNames");
	    try {
	    	request();
		rclient().setFuncGenNames(id(), names);
	    } catch (RemoteException e) {
		throw Xcept.wrap(e);
	    }
	}

	// new data query
	public ASQuery parseQuery(String s) throws Xcept {
	    try {
	    	return getASVar(s);
	    } catch (Xcept e) {
	    	return new RCQuery(this, s);
	    }
	} 
	public Data getData(int i, ASQuery s) throws Xcept {
	    checkBuilt("getData");
	    if (i >= nstores()) return null;
	    return stores[i].getData(s);
	}

	// get mult-opt data
	public MoptData getMoptData() throws Xcept {
	    return moptData;
	}

	// get profile
	public ProfileData getProfile() throws Xcept {
	    try {
		request();
	        checkBuilt("getProfile");
		return rclient().getProfile(id());
	    } catch (RemoteException e) {
		throw Xcept.wrap(e);
	    }
	}

	// simple query
	public OptimResults optimResults() { return optimResults; }
	public ASModel.Flags getFlags() { 
	    if (info == null) return new ASModel.Flags();
	    return info.flags; 
	}
	public ASInfo.JobInfo getJobInfo() { return jobInfo; }

	// get store name
	public String getStoreName(int i) {
	    if (i >= nstores()) return null;
	    return stores[i].name();
	}

	// needs remote implementation ???
	public int storeMode() { return storeMode; }

	// request
	protected void request() { client.request(); }

}



