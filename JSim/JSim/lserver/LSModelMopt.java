/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Local Server multiple optimization run & data

package JSim.lserver;

import JSim.util.*;
import JSim.jruntime.*;
import JSim.aserver.*; 
import JSim.data.*;
import JSim.nml.opt.*;

public class LSModelMopt implements MPDispatch.Monitor {
	private LSModel lsmodel; // for this model
	private ASInfo.Mopt info; // info for this job
	private int nproc;  // # processors available
	private ASQuery[] matchExprs; // exprs the match
	private boolean cancelled; // cancel flag
	private MoptData moptData; // created MOPT data

	// constructor
	public LSModelMopt(LSModel model, ASInfo.Mopt mopt,
	int nproc) throws Xcept {
	    lsmodel = model;
	    info = mopt;
	    this.nproc = nproc;
	    
	    // store match exprs
	    matchExprs = new ASQuery[nmatches()];
	    for (int m=0; m<nmatches(); m++) 
	    	matchExprs[m] = lsmodel.parseQuery(
		    info.optim.matchExprs[m]);
		    
	    // initialize data
	    moptData = new MoptData(info.optim.args.xname, 
	    	nsegments(), nmatches());
	}

	// run all segments in order
	public void runSimple() throws Xcept {
	    for (int s=0; s<nsegments() && !cancelled; s++) 
	    	runSegment(s, 0);
	}

	// create one job per segment,  run in batch (maybe MP)
	public void run() throws Xcept {

	    // preliminary single run in store 0
	    lsmodel.rtmodel().run(1, 1, 0, null);

	    // multiple optimization
	    MPDispatch.Job[] jobs = new MPDispatch.Job[nsegments()];
	    for (int s=0; s<nsegments(); s++) 
		jobs[s] = new SingleSegment(s);
	    MPDispatch dispatch = new MPDispatch("fim", jobs, this);
	    dispatch.run(nproc);    
	}

	// run one segment 
	private void runSegment(int s, int threadInxBase) 
	throws Xcept {

	    // run optimizer
	    ASInfo.Optim optInfo = info.optim.copy(); 
	    optInfo.refData = info.refData[s];
	    int np = 1; // # processor for this run
	    LSModelOptim optim = new LSModelOptim(lsmodel, optInfo,
		threadInxBase, np);
	    try {
		optim.optimize();
	    } catch (Exception e) {
		if (! info.noabort) throw Xcept.wrap(e);
		if (info.saveOptimResults)
		    moptData.setOptimResults(s, optim.results());
		moptData.setDone(s);
		return;
	    }

	    // store data 
	    //    doesn't appear to need MP synchronization
	    for (int p=0; p<npars(); p++) {
		double val = optim.results().bestX[p];
		moptData.setParData(p, s, val);
	    }
	    moptData.setRMSData(s, optim.results().bestErr);
	    for (int m=0; m<nmatches(); m++) {
	    	int storeInx = threadInxBase;
		Data data = lsmodel.getData(storeInx, matchExprs[m]);
		data.setName(info.refData[s][m].name());
		data.setDesc(null);
		moptData.setFitData(s, m, data);
	    }
	    if (info.saveOptimResults)
		moptData.setOptimResults(s, optim.results());
	    moptData.setDone(s);
	}
	
	// cancel
	public void cancel() {
	    cancelled = true;
	}

	// MPDispatch.Monitor method(s)
	public synchronized void jobStarted(MPDispatch.Job job) {
//	    System.err.println("mopt job started: " + job.jobName());
	}
	public synchronized void jobCompleted(MPDispatch.Job job) {
//	    System.err.println("mopt job done: " + job.jobName());
	}

	// simple query 
	public int nsegments() { return info.nsegments(); }
	public int npars() { return info.npars(); }
	public int nmatches() { return info.nmatches(); }
	public MoptData moptData() { 
	    return moptData; 
	}

	// single segment job
	public class SingleSegment implements MPDispatch.Job {
	    private int segment;
	    
	    // constructor
	    public SingleSegment(int segment) {
	    	this.segment = segment;
	    }
	    
	    // run this segment
	    public void jobRunX(int workerInx) throws Xcept {
	    	if (cancelled) return;
		int threadBaseInx = workerInx*2 + 1;
		runSegment(segment, threadBaseInx);
	    }
		
	    // skip
	    public void jobSkip() { /* ??? */ }

	    // job cancle
	    public void jobCancel() {
	    	cancelled = true;
	    }

	    // simple query
	    public String jobName() { 
	    	return "Segment_" + (segment+1);
	    }
	}
}

	
