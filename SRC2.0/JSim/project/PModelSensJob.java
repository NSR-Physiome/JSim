/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run model sensitivity analysis

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.aserver.*;

public class PModelSensJob extends PJob {

	// state
	private PModel pmodel;
	private ASModel rt;
	private ASInfo.Sens jobInfo;

	// constructor
	public PModelSensJob(PModel p) throws Xcept {
	    super(p, ASModel.SENS);
	    pmodel = p;
	    setName("Run model sensitivity " + pmodel.name());
	    rt = pmodel.rt();
	    jobInfo = pmodel.sens().makeJobInfo();
	    pmodel.timeStamp = System.currentTimeMillis();
	    baseVals = jobInfo.baseVals;
	}

	// may be interrupted
	public void stoppable() throws Xcept {
	    pmodel.setRunning(true);
	    setKillable(true);
	    pmodel.vars().updateFuncGenNames();
	    rt.sensRun(jobInfo);
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

	// query
	public String phaseDesc() {
	    ASInfo.Status stat = rt.getJobStatus();
	    if (stat == null) return "sens job status is null";
	    return "sensitivity " + (stat.nrunsDone+1) + "/" + 
	            stat.nruns;
	}
	public double phaseFrac() {
	    ASInfo.Status stat = rt.getJobStatus();
	    if (stat == null) return 0;
	    ASInfo.RunStatus rstat = stat.runStat(stat.nrunsDone);
	    if (rstat == null) return 0;
	    double frac = (rstat == null) ? 0 : rstat.frac;
	    return (stat.nrunsDone + frac) / stat.nruns;
	}
}

