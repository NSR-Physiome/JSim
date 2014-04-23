/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// run-time profiler report

package JSim.data;

import java.io.Serializable;
import java.text.*;
import java.util.*;
import JSim.util.*;

public class ProfileReport {
	private ProfileData prof;
	private static DateFormat dateFormat;

	private static final String[] PDECALLBACKNAMES =
	   new String[] { "LSFEA_tstep", "LSFEA_xstep",
	   	"MacCormack_State", "Toms731_State",
		"common_LHB", "common_RHB",
		"Toms731_LHB", "Toms731_RHB" };
	
	// constructor
	public ProfileReport(ProfileData prof) {
	    this.prof = prof;
	    if (dateFormat == null)
	    	dateFormat = new SimpleDateFormat("HH:mm:ss");
	}

	// create report
	public String getReport() {
	    StringBuffer s = new StringBuffer();
	    s.append("JSim Run-time Profile\n\n");
	    s.append("Job description: " + prof.desc + "\n");
	    String sstart = (prof.startTime == 0) ? 
	    	"unknown" : dateFormat.format(new Date(prof.startTime));
	    String sstop = (prof.stopTime == 0) ? 
	    	"unknown" : dateFormat.format(new Date(prof.stopTime));
	    long runtime = prof.stopTime - prof.startTime;
	    boolean hasRuntime = (runtime > 0);
	    String sruntime = hasRuntime ? ("" + runtime) : "unknown";
	    s.append("Job start time : " + sstart + "\n");
	    s.append("Job stop time  : " + sstop + "\n");
	    s.append("Run time (msec): " + sruntime + "\n");

	    // has PDE?
	    boolean hasPDE = false;
	    int nprobs = 
	        (prof.problems == null) ? 0 : prof.problems.length;    
	    for (int i=0; i<nprobs; i++) 
	    	if (prof.problems[i].type == ProfileData.PDE)
		    hasPDE = true;
	    
	    // problem table hdr
	    TextColumns cols = new TextColumns();
	    cols.addColumn(11, "type");
	    cols.addColumn(11, "block-ID");
	    cols.addColumn(11, "#state");
	    cols.addColumn(hasPDE ? 20 : 11, "#calls");
	    cols.addColumn(11, "#callbacks");
	    cols.addColumn(11, "cbs/call");
	    cols.addColumn(11, "msec/call");
	    cols.println();	    

	    // problem table entries
	    for (int i=0; i<nprobs; i++) {
	    	ProfileData.Problem prob = prof.problems[i];
		if (prob == null) continue;
		cols.print("" + typeName(prob.type));
		cols.print(prob.name);
		cols.print(prob.nstate);
		cols.print(prob.ncalls);
		cols.print(prob.ncallbacks);
		if (prob.ncalls > 0)
		   cols.print(div(prob.ncallbacks, prob.ncalls));
		else
		    cols.print("n/a");
		if (hasRuntime && prob.ncalls > 0)
		   cols.print(div(runtime, prob.ncalls));
		else
		    cols.print("n/a");
		cols.println();
		if (prob.type == ProfileData.PDE)
		    printPDECallbacks(cols, prob);
	    }
	    s.append("\nSolver Block Statistics:\n" + cols);
	    if (hasRuntime)
	    	s.append("          Note: msec/call assumes entire model run spent in solver block.");
	    
	    // done
	    return s.toString();
	}

	// problem type name
	public static String typeName(int type) {
	    switch (type) {
	    case ProfileData.ODE: return "ODE";
	    case ProfileData.PDE: return "PDE";
	    case ProfileData.FZERO1: return "LIN-ZERO";
	    case ProfileData.FZERO2: return "NONLIN-ZERO";
	    }
	    return "???";
	}

	// print PDE callback breakdown
	public void printPDECallbacks(TextColumns cols, ProfileData.Problem prob) {
	    if (prob.npdeCallbacks == null) return;
	    for (int i=0; i<prob.npdeCallbacks.length; i++) {
		long ncb = prob.npdeCallbacks[i];
		if (ncb == 0) continue;
	    	String name = (i <= PDECALLBACKNAMES.length) ?
		     PDECALLBACKNAMES[i] : "???";
		cols.print("");
		cols.print("");
		cols.print("");
		cols.print(name);
		cols.print(ncb);
		if (prob.ncalls > 0)
		    cols.print(div(ncb, prob.ncalls));
		else
		    cols.print("n/a");
		cols.println();
	    }
	}

	// long divide with rounding
	private long div(long a, long b) {
	    return (long) Math.round((a+0.0)/b);
	}

	// test harness
	public static void main(String[] args) throws Exception {
	    ProfileData d = new ProfileData();
	    d.desc = "loops run";
	    d.startTime = 123456;
	    d.problems = new ProfileData.Problem[2];
	    ProfileReport r = new ProfileReport(d);
	    String text = r.getReport();
	    System.out.println(text);
	}

}
