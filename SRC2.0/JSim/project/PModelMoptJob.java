/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// multiple optimization run job

package JSim.project; 

import JSim.aserver.*;
import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

public class PModelMoptJob extends PJob {

	// state
	private PModel pmodel;
	private ASInfo.Mopt jobInfo;

	// constructor
	public PModelMoptJob(PModel pmodel, ASInfo.Mopt jobInfo) 
	throws Xcept {
	    super(pmodel, ASModel.MOPT);
	    this.pmodel = pmodel;
	    setName("Run multiple optim " + pmodel.name());
	    this.jobInfo = jobInfo;
	    pmodel.timeStamp = System.currentTimeMillis();
	    setStackTrace(true);
	    baseVals = jobInfo.optim.baseVals;
	}

	// set stuff
	public void setSaveOptimResults(boolean b) {
	    jobInfo.saveOptimResults = b;
	}
	public void setNoAbort(boolean b) {
	    jobInfo.noabort = b;
	}
	public void setCalcCovMat(boolean b) {
	    jobInfo.optim.args.calcCovMat = b;
	}

	// may be interrupted
	public void stoppable() throws Xcept {
	    pmodel.setRunning(true);
	    setKillable(true);
	    pmodel.vars().updateFuncGenNames();
	    pmodel.rt().moptRun(jobInfo);
	    setKillable(false);
	}

	// postrun
	public void postRun() throws Xcept {
	    pmodel.setRunning(false);
	    switch (stat()) {
	    case NORMAL:
	    case XCEPT:
	    case OTHER:
	    	break;
	    default:
	    	pmodel.clearRT();
		break;
	    }
	}

	// query (local server only)
	public String phaseDesc() {
 	    try { 
	    	MoptData moptData = moptData();
	    	if (moptData == null) return "Segment 1";
	    	return "Segment " + (moptData.nsegmentsDone()+1);
	    } catch (Xcept e) { 
	    	return "Segment 1";
	    }
	}
	public double phaseFrac() {
	    try { 
	    	MoptData moptData = moptData();
	    	if (moptData == null) return 0;
	 	return (double) moptData.nsegmentsDone() 
		    / moptData.nsegments();
	    } catch (Xcept e) { 
	    	return 0;
	    }
	}

	// simple query
	public MoptData moptData() throws Xcept { 
	    return pmodel.rt().getMoptData(); 
	}
}

