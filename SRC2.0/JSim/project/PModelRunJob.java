/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Run model once through

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

public class PModelRunJob extends PJob {

	// state
	private PModel pmodel;
	private ASModel rt;
	
	// constructors
	public PModelRunJob(PModel p) throws Xcept {
	    super(p, ASModel.SINGLE);
	    pmodel = p;
	    setName("Run model " + pmodel.name());
	    rt = pmodel.rt();
	    PModelVars vars = pmodel.vars();
	    pmodel.timeStamp = System.currentTimeMillis();
	    pmodel.vars().updateFuncGenNames();
	    baseVals = pmodel.makeJobInfo();
	}

	// may be interrupted
	public void stoppable() throws Xcept {
	    pmodel.setRunning(true);
	    setKillable(true);
	    rt.singleRun(baseVals);
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
	public ASModel rt() { return rt; }
	public String phaseDesc() {
	    ASInfo.Status stat = rt.getJobStatus();
            if (stat == null) return "phase 0";
	    ASInfo.RunStatus rstat = stat.runStat(0);
	    if (rstat == null) return "phase 0";
	    return "phase " + rstat.phase;	
	}
	public double phaseFrac() {
	    ASInfo.Status stat = rt.getJobStatus();
            if (stat == null) return 0;
	    ASInfo.RunStatus rstat = stat.runStat(0);
	    if (rstat == null) return 0;
	    return rstat.frac;
	}

}
