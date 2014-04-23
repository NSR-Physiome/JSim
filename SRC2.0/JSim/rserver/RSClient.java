/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// server side connection for 1 client

package JSim.rserver;

import java.io.*;
import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import JSim.rclient.*;

public class RSClient 
implements RSClientIF, RCClientIF, ASServer.Messenger {
	private RSServerIF rserver; // server 
	private ASServer lserver; // does real work
	private RSInfo.Client info; // queriable info
	private RSModel.NList models; // run-time models
	private RSJob currJob;	// current job, if any
	private RCInfo.JobStatus prevJobStatus;  // previous job status, if any
	private Serializable prevJobOutput;  // previous job output, if any
	private boolean disconnected; // true during shutdown

	// constructor
	public RSClient(RSServerIF s, RSInfo.Client infox) 
	throws RemoteException {
	    info = infox;
	    if (info.userPort > 0) 
	    	UnicastRemoteObject.exportObject(this, info.userPort);
	    else
	    	UnicastRemoteObject.exportObject(this);
	    rserver = s;
	    models = new RSModel.NList(2);
	    request();
	    info.startTime = info.requestTime;
	    try {
		lserver = createLocalServer();
	    } catch (Xcept e) {
		throw new RemoteException(e.getMessage());
	    }
	}

	// create local server for this client
	private ASServer createLocalServer() throws Xcept {
	    NamedVal.NList nvals = new NamedVal.NList();
	    nvals.setVal("buildDir", info.workDir.getPath());
	    ASInfo.Sandbox sandbox = null;
	    if (info.sandbox) {
	    	sandbox = new ASInfo.Sandbox();
		sandbox.hosts.add(info.host);
	    }
	    return ASServer.create(nvals, this, sandbox);
	}		

	// simple query
	public String id() { return info.id; }
	public String name() { return id(); }
	public String diagInfo() { return "RSClient " + id(); }
	public ASServer lserver() { return lserver; }
	public RSInfo.Client getInfo() {
	    info.jvmID = RSAdmin.jvmID();
	    info.maxMemory = Runtime.getRuntime().maxMemory();
	    info.currMemory = Runtime.getRuntime().totalMemory();
	    info.job = null;
	    if (currJob != null) 
		info.job = currJob.getStatus();
	    info.models = new RSInfo.Model[models.size()];
	    for (int i=0; i<models.size(); i++) 
		info.models[i] = models.model(i).getInfo();
	    return info; 
	}
	public OptimAlg.Info[] optimAlgsInfo() 
	throws RemoteException {
	    try {
	    	return lserver.optimAlgsInfo();
	    } catch (Xcept e) {
	    	throw new RemoteException(e.getMessage());
	    }
	}

	// add model to this client
	public void addModel(RSModel model) {
	    log("  adding model " + model.id());
	    models.add(model);
	}

	// remove model from client,  if exists
	public void removeModel(String id) {
	    RSModel m = models.model(id);
	    if (m != null) {
		models.remove(id);
		log("  removing previous version of model " + id);
	    }
	}

	//// Remote methods

	// query model,  may be null if doesn't exist
	public RSModel model(String id) throws RemoteException {
	    return models.model(id); 
	}

	// ping request from RCClient
	public void ping() throws RemoteException {
	    request();
	}

	// translate model text
	public String translateModelText(int srcType, 
	int destType, String srcText, String options) 
	throws RemoteException {
	    try {
	    	request();
		return lserver.translateModelText(
		    srcType, destType, srcText, options);
	    } catch (Exception e) {
	    	throw wrap("translateModelText", e);
	    }
	}

	// get tranlator warnings
	public String[] getTranslatorWarnings() 
	throws RemoteException {
	    try {
	    	request();
		return lserver.getTranslatorWarnings();
	    } catch (Exception e) {
	    	throw wrap("getTranslatorWarnings", e);
	    }
	}

	// start model build
	public RCInfo.JobStatus startBuild(String modelID, 
	ASInfo.BuildInfo binfo) throws RemoteException {
	    try {
	    	request();
	    	RSJob job = new RSJob.Build(this, modelID, binfo);
	    	startJob(job);
	    	return job.getStatus();
	    } catch (Exception e) {
		throw wrap("startBuild", e);
	    }
	}

	// start single run, return jobID
	public RCInfo.JobStatus startSingleRun(String modelID, RCInfo.RunInput rinfo) 
	throws RemoteException {
	    try {
	    	request();
	    	RSJob job = new RSJob.SingleRun(this, modelID, rinfo);
	    	startJob(job);
	    	return job.getStatus();
	    } catch (Exception e) {
		throw wrap("startSingleRun", e);
	    }
	}

	// start loops run, return jobID
	public RCInfo.JobStatus startLoopsRun(String modelID, RCInfo.LoopsInput rinfo) 
	throws RemoteException {
	    try {
	    	request();
	    	RSJob job = new RSJob.LoopsRun(this, modelID, rinfo);
	    	startJob(job);
	    	return job.getStatus();
	    } catch (Exception e) {
		throw wrap("startLoopsRun", e);
	    }
	}

	// start sensitivity run, return jobID
	public RCInfo.JobStatus startSensRun(String modelID, RCInfo.SensInput rinfo) 
	throws RemoteException {
	    try {
	    	request();
	    	RSJob job = new RSJob.SensRun(this, modelID, rinfo);
	    	startJob(job);
	    	return job.getStatus();
	    } catch (Exception e) {
		throw wrap("startSensRun", e);
	    }
	}

	// start optim run, return jobID
	public RCInfo.JobStatus startOptimRun(String modelID, RCInfo.OptimInput rinfo) 
	throws RemoteException {
	    try {
	    	request();
	    	RSJob job = new RSJob.OptimRun(this, modelID, rinfo);
	    	startJob(job);
	    	return job.getStatus();
	    } catch (Exception e) {
		throw wrap("startOptimRun", e);
	    }
	}

	// start multi-optim run, return jobID
	public RCInfo.JobStatus startMoptRun(String modelID, RCInfo.MoptInput rinfo) 
	throws RemoteException {
	    try {
	    	request();
	    	RSJob job = new RSJob.MoptRun(this, modelID, rinfo);
	    	startJob(job);
	    	return job.getStatus();
	    } catch (Exception e) {
		throw wrap("startMoptRun", e);
	    }
	}

	// job status
	public RCInfo.JobStatus getJobStatus() 
	throws RemoteException {
	    try {
	    	request();
	    	if (currJob != null) return currJob.getStatus();
	    	return prevJobStatus;
	    } catch (Exception e) {
e.printStackTrace();
		throw wrap("getJobStatus", e);
	    }
	}

	// skip to next loop
	public RCInfo.JobStatus nextLoop() throws RemoteException {
	    try {
	    	request();
	    	if (currJob != null)
	    	    currJob.nextLoop();
	    	return prevJobStatus;
	    } catch (Exception e) {
		throw wrap("nextLoop", e);
	    }
	}

	// cancel job
	public RCInfo.JobStatus cancelJob() throws RemoteException {
	    try {
	    	request();
	    	if (currJob == null) throw new RemoteException(
		    "No current job to cancel on remote server");
	    	currJob.cancel();
	    	return prevJobStatus;
	    } catch (Exception e) {
		throw wrap("cancelJob", e);
	    }
	}

	// job output
	public Serializable getJobOutput(String id) 
	throws RemoteException {
	    try {
	    	request();
	    	if (prevJobStatus != null && id.equals(prevJobStatus.id))
		    return prevJobOutput;
	    	if (currJob != null && id.equals(currJob.id())) 
		    throw new RemoteException(
		    	"Job " + id + " not yet completed on server");
	    	throw new RemoteException(
		    "Job " + id + " does not exist on server");
	    } catch (Exception e) {
		throw wrap("getJobOutput", e);
	    }
	}

	// get model build warnings
	public String[] getBuildAlerts(String modelID) 
	throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) return null;
	    	return model.getBuildAlerts();
	    } catch (Exception e) {
		throw wrap("getBuildAlerts", e);
	    }
	}

	// get model text
	public String getText(String modelID, int type, String variant) 
	throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) return null;
	    	return model.getText(type, variant);
	    } catch (Exception e) {
		throw wrap("getText", e);
	    }
	}

	// get model text warnings
	public String[] getTextWarnings(String modelID, int type, String variant) 
	throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) return null;
	    	return model.getTextWarnings(type, variant);
	    } catch (Exception e) {
		throw wrap("getTextWarnings", e);
	    }
	}

	// get common text
	public String getCommonText(String name) throws RemoteException {
	    try {
	    	request();
	    	return lserver.getCommonText(name);
	    } catch (Exception e) {
		throw wrap("getCommonText", e);
	    }
	}

	// get Solver defaults
	public NamedVal[] getSolverDefaults() throws RemoteException {
	    try {
	    	request();
		return lserver.getSolverDefaults().info();
	    } catch (Exception e) {
		throw wrap("getSolverDefaults", e);
	    }
	}

	// set assignment
	public void setAssign(String modelID, String vname, String
	expr) throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) throw new RemoteException(
		    "Remote model not found: " + modelID);
	    	model.setAssign(vname, expr);
	    } catch (Exception e) {
		throw wrap("setAssign", e);
	    }
	}

	// parse query
	public RCInfo.Query parseQuery(String modelID, String query)
	throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) throw new RemoteException(
		    "Remote model not found: " + modelID);
	    	return model.parseQuery(query);
	    } catch (Exception e) {
		throw wrap("parseQuery", e, false); 
	    }
	}
	    
	// get run data
	public DataInfo getData(String modelID, 
	int run, String expr) throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) return null;
	    	return model.getData(run, expr);
	    } catch (Exception e) {
		throw wrap("getData", e, false); 
	    }
	}

	// get profile
	public ProfileData getProfile(String modelID) throws RemoteException {
	    try {
	    	request();
	    	RSModel model = model(modelID);
	    	if (model == null) return null;
	    	return model.getProfile();
	    } catch (Exception e) {
		throw wrap("getProfile", e); 
	    }
	}

	// set function generators
	public void setFuncGenNames(String modelID, String[] names) 
	throws RemoteException {
	    try {
	    	request();
		RSModel model = model(modelID);
		if (model == null) return;
		model.setFuncGenNames(names);
	    } catch (Exception e) {
		throw wrap("setFuncGenNames", e);
	    }
	}

	// disconnect
	public void disconnect() throws RemoteException {
	    try {
	    	request();
	    	// stop any running processes ???
	    	removeWorkDir();
	    	rserver.disconnectID(info.id);
	    	disconnected = true;	    
	    } catch (Exception e) {
		throw wrap("disconnect", e);
	    }
	}

	// disconnected?
	public boolean disconnected() throws RemoteException { 
	    return disconnected; 
	}

	//// local support routines

	// rethrow unexpected exception
	public RemoteException wrap(String msg, Exception e) {
	    return wrap(msg, e, true);
	}
	public RemoteException wrap(String msg, Exception e,
	boolean logit) {
	    msg = msg + ": ";
	    if (logit) log(msg, e);
	    if (e instanceof RemoteException)
		return (RemoteException) e;
	    if (e instanceof Xcept)
		return new RemoteException(
		    ((Xcept) e).cleanMessage());
	    return new RemoteException(msg + e);
	}

	// start a job
	private void startJob(RSJob job) throws RemoteException {
	    if (currJob != null) throw new RemoteException(
	    	"Job " + currJob.name() + " in progress" + 
	    	", can't start new job " + job);
	    currJob = job;
	    try {
	    	currJob.start(); // should be in async thread
	    } catch (IllegalThreadStateException e) {
	        log("", e);
		throw new RemoteException("" + e);
	    }
	}

	// called by RSJob when currJob terminates
	public void jobDone() throws Xcept {
	    prevJobOutput = null;
	    prevJobStatus = currJob.getStatus();
	    if (currJob.info.termStat == RCInfo.NORMAL) 
	    	prevJobOutput = currJob.getOutput();
	    prevJobStatus.done = true;
	    currJob = null;
	}

	// remove work directory
	private void removeWorkDir() {
	    File dir = info.workDir;
	    log("deleting work directory " + dir.getAbsolutePath());
	    UtilIO.deleteDir1(dir);
	}

	// log an error message
	public void log(String msg) {
	    log(msg, null);
	}
	public void log(String msg, Exception e) {
	    msg = id() + ": " + msg;
	    StackTraceElement[] stack = null;	    
	    if (e != null) {
	    	msg = msg + " " + e;
	    	stack = e.getStackTrace();
	    } 
	    try {
		rserver.log(msg, stack);
	    } catch (RemoteException re) {
		System.err.println(msg);
		System.err.println(re);
		re.printStackTrace(System.err);
	    }
	}

	// note time of client request 
	protected void request() {
	    info.requestTime = System.currentTimeMillis();
	}

	// print ASServer.Messenger message from server
	//    ??? this should probably go to a log file instead
	public void message(ASInfo.Message msg) {
	    String s = msg.warning ? "warning: " : "";
	    s = s + msg.text;
	    System.err.println(s);
	}

	// memory message
	public String getMemoryMessage() throws RemoteException {
	    return Util.memoryMessage();
	}
}

