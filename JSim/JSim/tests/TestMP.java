/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// test multiple concurrent runs

package JSim.tests; import JSim.text.*;

import java.io.*;	// File and PrintStream

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; import JSim.project.*;

public class TestMP {

	// mainline
	public static final void main(String[] args)
	throws Exception {
	    Appl appl = new Appl(args);
	    Project project = new Project("proj", appl);
	    project.importXML(appl.projFile);
	    PNamed.List models = project.descendants(PModel.class);

	    // build all models, create run jobs
	    for (int i=0; i<models.size(); i++) {
		PModel pmodel = (PModel) models.get(i);
		System.err.println("building model " + 
		    pmodel.name());
		PJob pjob = new PModelBuildJob(pmodel);
		pjob.setStackTrace(true);
		pjob.simpleRun();
	    }

	    // start runs
	    PJob[] pjobs = new PJob[models.size()];
	    for (int i=0; i<models.size(); i++) {
		PModel pmodel = (PModel) models.get(i);
		System.err.println("starting run " + 
		    pmodel.name());
		pjobs[i] = new PModelRunJob(pmodel);
		pjobs[i].start();
	    }

	    // wait for runs to complete
	    for (int i=0; i<pjobs.length; i++) 
		pjobs[i].join();
	    System.err.println("runs completed");

	    // collect output data
	    if (appl.vname == null) return;
	    Data.List dlist = new Data.List(4);
	    for (int i=0; i<models.size(); i++) {
		PModel pmodel = (PModel) models.get(i);
	    	ASModel rt = pmodel.rt();
		ASQuery expr = rt.parseQuery(appl.vname);
		Data data = rt.getData(0, expr);
		data.setGroup(pmodel.name());
		dlist.add(data);
	    }

	    // write output data
	    DataFormat fmt = new PrettyDataFormat();
	    DataWriter wrt = fmt.createWriter();
	    wrt.writeData(System.out, dlist);
	}

	// application class
	public static class Appl extends PApplication {
	    public String vname;

	    // constructor
	    public Appl(String[] args) throws Xcept {
		super(args);
	    }

	    // parse extra args
	    protected void parse_xargs(String[] args) throws Xcept {
		for (int i=0; i<args.length; i++) {
		    if (args[i].equals("-o")) {
			i++;
		    	vname = args[i++];
		    }
		}
	    }
	}
}

