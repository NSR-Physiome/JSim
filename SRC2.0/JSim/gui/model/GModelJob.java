/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// model PJob with graphic update

package JSim.gui.model;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

abstract public class GModelJob extends GJob {
	protected GModel gmodel;
	protected PModel pmodel;

	// constructor
	public GModelJob(GModel m) {
	    super(m);
	    gmodel = m;
	    pmodel = gmodel.pmodel();
	}

	// pre-processing for all jobs
	protected void gpre() {
//	    gmodel.setEditable(false); 
// line above causes text caret to disappear on some compiler errors
	    gmodel.gproject().jobStartStop(pjob, true);
	    gmodel.refresh();
	}

	// post processing for all but Build job
	protected void gpost() {
	    if (! pmodel.rt().isBuilt()) 
		gmodel.gpars().connectRT();
	    gproject().project().revalidate();
	    gmodel.setEditable(true);
	    gmodel.gproject().jobStartStop(pjob, false);
	    gproject().refresh();
	}

	// Build job
	public static class Build extends GModelJob {
	    private boolean saveChanged;
	    private boolean demoMode;

	    // constructor
	    public Build(GModel m, boolean demo) throws Xcept {
		super(m);
		saveChanged = gproject().project().changed;
		pjob = new PModelBuildJob(pmodel);
		demoMode = demo;
	    }

	    // post processing
	    protected void gpost() {
		try {
		    boolean hasError = pjob.stat() != PJob.NORMAL;
	    	    String[] alerts = pmodel.rt().getBuildAlerts();
		    gproject().messages(hasError, alerts);
		} catch (Xcept e) {
		    gproject().message(e);
		}
	    	gmodel.gpars().connectRT();
	    	gproject().project().revalidate();
	    	gmodel.setEditable(true);
	    	gmodel.realizeCustom();
	        if (demoMode) gproject().demo(gmodel);
	 	gmodel.gproject().jobStartStop(pjob, false);
	    	gproject().refresh();
	    	if (pmodel.rt().isBuilt())
		    gmodel.showParsTab();
		else 
		    gmodel.gsrc().setCaret(this);
		gproject().project().changed = saveChanged;
	    }
	}		


	// single run job
	public static class SingleRun extends GModelJob {
	    public SingleRun(GModel m) throws Xcept {
		super(m);
		pjob = new PModelRunJob(pmodel);
	    }
	}		

	// loops job
	public static class Loops extends GModelJob {
	    public Loops(GModel m) throws Xcept {
		super(m);
		pjob = new PModelLoopsJob(pmodel);
	    }
	}		

	// sensitivity job
	public static class Sens extends GModelJob {
	    public Sens(GModel m) throws Xcept {
		super(m);
		pjob = new PModelSensJob(pmodel);
	    }
	}		

	// optimizer job
	public static class Optim extends GModelJob {
	    public Optim(GModel m) throws Xcept {
		super(m);
		pjob = new PModelOptimJob(pmodel);
	    }
	}		

	// Monte Carlo job
	public static class Monte extends GModelJob {
	    public Monte(GModel m) throws Xcept {
		super(m);
		pjob = pmodel.monte().createJob();
	    }
	}		
}
