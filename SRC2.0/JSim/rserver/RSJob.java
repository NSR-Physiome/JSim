/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// job running on remote server

package JSim.rserver;

import java.io.*;
import java.rmi.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.rclient.*;

public abstract class RSJob extends Thread implements Named {
	// static fields
	private static int ct = 0;	// # jobs

	// instance fields
	protected RSClient rclient;	// job for this client
	protected RCInfo.JobStatus info;// status info
	protected ASModel rt;      // run-time model does work

	// constructor
	public RSJob(RSClient c, String d) {
	    rclient = c;
	    info = new RCInfo.JobStatus();
	    info.id = rclient.id() + "." + ct++;
	    info.desc = d;
	    info.done = false;
	    info.termMessage = null;
	    info.termStat = RCInfo.RUNNING;
	}

	// query
	public String id() { return info.id; }
	public String name() { return info.id; }
	public String diagInfo() { return "RSJob " + name(); }
	public RCInfo.JobStatus getStatus() {
	    if (rt != null) {
	    	info.jstat = rt.getJobStatus();
	    }
	    return info; 
	}

	// run this job
	public void run() {
	    info.startTime = System.currentTimeMillis();
	    rclient.log("Starting job " + id() + " " + info.desc);
	    try {
	    	run0();
		info.termStat = RCInfo.NORMAL;
		info.termMessage = "terminated normally";
	    	rclient.log("Job " + id() + " terminated normally");
	    } catch (Xcept e) {
		info.termStat = RCInfo.XCEPT;
		info.termMessage = e.cleanMessage();
	    	rclient.log("Job " + id() + " exception:\n\t" +
		    e.cleanMessage());
	    } catch (Exception e1) {
	        info.termStat = RCInfo.OTHER;
		info.termMessage = "" + e1;
	    	rclient.log("Job " + id() + " exception:\n\t" + e1);
	    }
	    info.doneTime = System.currentTimeMillis();
	    try {
	    	rclient.jobDone();
	    } catch (Xcept e) {
		rclient.log("RSClient.jobDone: ", e);
	    }
	}

	// skip to next loop in loops job
	public void nextLoop() throws RemoteException {
	    rclient.log("skipping to next loop " + id());
	    rt.nextLoop();
	}	

	// cancel job thread
	public void cancel() throws RemoteException {
	    rclient.log("canceling job " + id() + " at user request");
	    rt.cancelJob();
	    try {
		join();
	    } catch (Exception e) {
		rclient.log("", e);
		throw new RemoteException("" + e);
	    }
	    rclient.log("  job cancel " + id() + " completed");
	}

	// specific tasks for this job
	abstract public void run0() throws Xcept;
	abstract public Serializable getOutput() throws Xcept;

	// build model sub-class
	public static class Build extends RSJob {
	    private String modelID;
	    private ASInfo.Build binfo;
	    private RSModel rsmodel; 

	    // constructor
	    public Build(RSClient c, String id, 
	    ASInfo.BuildInfo b) throws RemoteException {
		super(c, "build model " + id);
		modelID = id;
		try {
		    binfo = b.build();
		    binfo.options.setVal("traceVars", ""); // no remote support
		    binfo.options.setVal("traceNaN", false); // no remote support
		} catch (Xcept e) {
		    throw new RemoteException("" + e);
		}
	    }

	    // run job
	    public void run0() throws Xcept {
		rclient.removeModel(modelID); // remove old model, if necessary
		rt = rclient.lserver().newModelRT();
		rt.buildRT(binfo);

		rsmodel = new RSModel(rclient, rt, modelID);
		rclient.addModel(rsmodel);
	    }

	    // job-specific output info
	    public Serializable getOutput() throws Xcept { 
		return new RCInfo.Model(rsmodel.id(), rt);
	    }
	}

	//  model single run sub-class
	public static class SingleRun extends RSJob {
	    RSModel rsmodel;
	    RCInfo.RunInput rinfo;

	    // constructor
	    public SingleRun(RSClient c, String modelID, 
	    RCInfo.RunInput r) throws RemoteException {
		super(c, "single run for model " + modelID);
		rsmodel = rclient.model(modelID);
		if (rsmodel == null) throw new RemoteException(
		    "ModelID " + modelID + " not found on remote server");
		rinfo = r;
	    }

	    // run job
	    public void run0() throws Xcept {
		rsmodel.clearStores(1);
		rt = rsmodel.rt();
		rsmodel.loadRunInput(rinfo);
		NamedVal.NList runVals = 
		    new NamedVal.NList(rinfo.runVals);
		rt.singleRun(runVals);
		rsmodel.runDone();		
	    }

	    // bundle store deltas
	    public RCInfo.JobStatus getStatus() {
		info.jobInfo = rsmodel.getDeltas();
	    	return super.getStatus(); 
	    }

	    // job-specific output info
	    public Serializable getOutput() throws Xcept {
		return new RCInfo.RunOutput(0, rt);
	    }
	}

	//  model loops run sub-class
	public static class LoopsRun extends RSJob {
	    RSModel rsmodel;
	    RCInfo.LoopsInput rinfo;

	    // constructor
	    public LoopsRun(RSClient c, String modelID, 
	    RCInfo.LoopsInput r) throws RemoteException {
		super(c, "loops run for model " + modelID);
		rsmodel = rclient.model(modelID);
		if (rsmodel == null) throw new RemoteException(
		    "ModelID " + modelID + " not found on remote server");
		rinfo = r;
	    }

	    // run job
	    public void run0() throws Xcept {
		rsmodel.clearStores(rinfo.loopsInfo.runNames.length);
		rt = rsmodel.rt();
		rsmodel.loadRunInput(rinfo.runInput);
		ASInfo.Loops loops = rinfo.loopsInfo.loops();
		rt.loopsRun(loops);
		rsmodel.runDone();		
	    }

	    // bundle store deltas
	    public RCInfo.JobStatus getStatus() {
		info.jobInfo = rsmodel.getDeltas();
	    	return super.getStatus(); 
	    }

	    // job-specific output info
	    public Serializable getOutput() throws Xcept {
		return new RCInfo.RunOutput(0, rt);
	    }
	}

	//  model sensitivity run sub-class
	public static class SensRun extends RSJob {
	    RSModel rsmodel;
	    RCInfo.SensInput rinfo;

	    // constructor
	    public SensRun(RSClient c, String modelID, 
	    RCInfo.SensInput r) throws RemoteException {
		super(c, "sensitivity run for model " + modelID);
		rsmodel = rclient.model(modelID);
		if (rsmodel == null) throw new RemoteException(
		    "ModelID " + modelID + " not found on remote server");
		rinfo = r;
	    }

	    // run job
	    public void run0() throws Xcept {
		rsmodel.clearStores(rinfo.sens.parNames.length + 1);
		rt = rsmodel.rt();
		rsmodel.loadRunInput(rinfo.runInput);
		ASInfo.Sens sens = rinfo.sens.sens();
		rt.sensRun(sens);
		rsmodel.runDone();		
	    }

	    // bundle store deltas
	    public RCInfo.JobStatus getStatus() {
		info.jobInfo = rsmodel.getDeltas();
	    	return super.getStatus(); 
	    }

	    // job-specific output info
	    public Serializable getOutput() throws Xcept {
		return new RCInfo.RunOutput(0, rt);
	    }
	}

	//  model optim run sub-class
	public static class OptimRun extends RSJob {
	    RSModel rsmodel;
	    RCInfo.OptimInput rinfo;

	    // constructor
	    public OptimRun(RSClient c, String modelID, 
	    RCInfo.OptimInput r) throws RemoteException {
		super(c, "optimization run for model " + modelID);
		rsmodel = rclient.model(modelID);
		if (rsmodel == null) throw new RemoteException(
		    "ModelID " + modelID + " not found on remote server");
		rinfo = r;
	    }

	    // run job
	    public void run0() throws Xcept {
		rsmodel.clearStores(0); // no live updates
		rt = rsmodel.rt();
		rsmodel.loadRunInput(rinfo.runInput);
		ASInfo.Optim optim = rinfo.optimInfo.optim();
		rt.optimRun(optim);
		rsmodel.runDone();		
	    }

	    // no live updates
	    public RCInfo.JobStatus getStatus() {
		info.jobInfo = null; // no live updates
		try {
		    info.jobInfo = getOutput();
		} catch (Exception e) {
		    System.err.println("RSJob.getStatus: " + e);
		}
	    	return super.getStatus(); 
	    }

	    // job-specific output info
	    public Serializable getOutput() throws Xcept {
		return getOutput(true, true);
	    }
	    public Serializable getOutput(boolean optR, boolean runOut)
	    throws Xcept {
	        if (rt == null) return null;
		if (!optR && !runOut) return null;
		RCInfo.OptimOutput oo = new RCInfo.OptimOutput(); 
		if (optR) {
		    OptimResults res =  rt.optimResults();
		    if (res != null) 
		        oo.orinfo = res.info();
		}
		if (runOut)
		    oo.runOutput = new RCInfo.RunOutput(0, rt);
		return oo;
	    }
	}

	//  model mult-optim run sub-class
	public static class MoptRun extends RSJob {
	    RSModel rsmodel;
	    RCInfo.MoptInput rinfo;
	    boolean[] lastMoptDone;

	    // constructor
	    public MoptRun(RSClient c, String modelID, 
	    RCInfo.MoptInput r) throws RemoteException {
		super(c, "multi-optimization run for model " + modelID);
		rsmodel = rclient.model(modelID);
		if (rsmodel == null) throw new RemoteException(
		    "ModelID " + modelID + " not found on remote server");
		rinfo = r;
	    }

	    // run job
	    public void run0() throws Xcept {
		rsmodel.clearStores(0); // no live updates
		rt = rsmodel.rt();
		rsmodel.loadRunInput(rinfo.runInput);
		ASInfo.Mopt mopt = rinfo.moptInfo.mopt();
		rt.moptRun(mopt);
		rsmodel.runDone();		
	    }

	    // no live updates
	    public RCInfo.JobStatus getStatus() {
		info.jobInfo = null; // no live updates
		try {
		    info.jobInfo = getOutput();
		} catch (Exception e) {
		    System.err.println("RSJob.getStatus: " + e);
		}
	    	return super.getStatus(); 
	    }

	    // job-specific output info
	    public Serializable getOutput() throws Xcept {
		return getOutput(true, true);
	    }
	    public Serializable getOutput(boolean moptR, boolean runOut)
	    throws Xcept {
	        if (rt == null) return null;
		if (!moptR && !runOut) return null;
		RCInfo.MoptOutput moo = new RCInfo.MoptOutput(); 
		if (moptR) {
		    MoptData mdata = rt.getMoptData();
		    if (mdata != null) {
			boolean[] tempDone = mdata.segmentsDone();
		        moo.morinfo = new MoptData.Info(mdata, lastMoptDone);
			lastMoptDone = tempDone.clone();
		    }
		}
		if (runOut)
		    moo.runOutput = new RCInfo.RunOutput(0, rt);
		return moo;
	    }
	}

}
