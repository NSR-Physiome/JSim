/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run model set of model optim

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

public class PModelOptimJob extends PJob {

	// state
	private PModel pmodel;
	private ASModel rt;
	private ASInfo.Optim jobInfo;

	// constructor
	public PModelOptimJob(PModel p) throws Xcept {
	    super(p, ASModel.OPTIM);
	    pmodel = p;
	    setName("Run model optimizer " + pmodel.name());
	    rt = pmodel.rt();
	    jobInfo = pmodel.optim().makeJobInfo();
	    pmodel.timeStamp = System.currentTimeMillis();
	    baseVals = jobInfo.baseVals;
	}

	// may be interrupted
	public void stoppable() throws Xcept {
	    pmodel.setRunning(true);
	    setKillable(true);
	    pmodel.vars().updateFuncGenNames();
	    rt.optimRun(jobInfo);
	    setKillable(false);
	}

	// postrun
	public void postRun() throws Xcept {
	    pmodel.setRunning(false);
	    switch (stat()) {
	    case NORMAL:
	    case XCEPT:
	    case OTHER:
	        updateBest();
	    	break;
	    default:
	    	pmodel.clearRT();
		break;
	    }
	}

	// update best par values, if appropriate
	public void updateBest() throws Xcept {
	    OptimResults results = rt.optimResults();
	    if (results == null) return;
	    if (results.status == OptimResults.ERROR) return;
	    
	    for (int i=0; i<results.nx(); i++) {
	        if (Double.isNaN(results.bestX[i])) continue;
	    	String n = results.args.xname[i];
		Control cntl = pmodel.vars().control(n);
		if (cntl == null) throw new Xcept(pmodel,
		    "updating parameter <" + n + "> not found");
//		if (! (cntl instanceof AssignControl)) throw new Xcept(
//		    cntl, "Optimizer can only update AssignControls");
//		((AssignControl) cntl).setValFromOptim(results.bestX[i]);
		int prec = pmodel.optim().reportPrec.val();
		if (prec < 3 || prec > 16)
		    prec = 4; 
		String sbestX = PrettyFormat.sformat(results.bestX[i], prec);
		cntl.setVal(sbestX);
	    }
	}

	// query
	public String phaseDesc() {
	    ASInfo.Status stat = rt.getJobStatus();
	    if (stat == null) return "optim job status is null";
	    	return "run " + (stat.nrunsDone+1) + "/" + 
	            (stat.nruns-1); // nruns is padded by 1, HACK!!!
	}
	public double phaseFrac() {
	    ASInfo.Status stat = rt.getJobStatus();
	    if (stat == null) return 0;
	    ASInfo.RunStatus rstat = stat.runStat(1);
	    if (rstat == null) return 0;
	    double frac = (rstat == null) ? 0 : rstat.frac;
	    return (stat.nrunsDone + frac) / stat.nruns;
	}
}

