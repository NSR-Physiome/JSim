/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Build model job

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.aserver.*;

public class PModelBuildJob extends PJob {

	// state
	private ASInfo.Build buildInfo;
	private XceptPos mmlPos; // MML error position

	// constructor
	public PModelBuildJob(PModel p) throws Xcept {
	    super(p, ASModel.COMPILE);
	    setName("Compile model " + pmodel().name());
	    pmodel().preBuild();
	    pmodel().timeStamp = 0;
	    buildInfo = p.createBuildInfo();
	}

	// may be interrupted
	public void stoppable() throws Xcept {
	    ASServer server = pmodel().server();
	    setKillable(true);
	    try {
		pmodel().rt().buildRT(buildInfo);
		pmodel().vars().updateFuncGenNames();
	    } catch (Xcept e) {
		mmlPos = e.pos;
		throw e;
	    }
	}

	// postrun
	public void postRun() throws Xcept {
	    if (stat() == NORMAL && pmodel().rt().isBuilt()) 
		pmodel().postBuild();
	    else
		pmodel().clearRT();
	}

	// process Xcept message (multi-line)
	public String xceptMessage(Xcept e) {
	    return e.getMessage();
	}

	// query
	public XceptPos mmlPos() { return mmlPos; }
	public String phaseDesc() { return null; }
	public double phaseFrac() { return 0; }
}
