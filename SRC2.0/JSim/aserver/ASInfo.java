/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// run-time job control information

package JSim.aserver;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;

public class ASInfo {

	// Sandbox (Server SecurityManager)
	public static class Sandbox  {
	    public StringList readPath; // allow dir/file reads
	    public StringList writePath; // allow dir/file writes
	    public StringList hosts; // allow host communication
	    public boolean readGUI; // make standard GUI classes readable
	    public Sandbox() {
	    	readPath = new StringList();
		writePath = new StringList();
		hosts = new StringList();
	    }
	}

	// general catchall for JobInfo
	public static interface JobInfo {
	}

	// Build job
	public static class Build implements JobInfo {
	    public String name; 
	    public String modelSource;
	    public int sourceType;
	    public NamedVal.NList options; // user options
	    public Build() { }
	}

	// Serializable build info
	public static class BuildInfo implements Serializable {
	    public String name; 
	    public String modelSource;
	    public int sourceType;
	    public NamedVal[] options; // user options
	    public BuildInfo(Build b) throws Xcept {
	    	name = b.name;
		modelSource = b.modelSource;
		sourceType = b.sourceType;
		if (b.options != null) 
		    options = b.options.info();
	    }
	    public Build build() throws Xcept {
	    	Build b = new Build();
	    	b.name = name;
		b.modelSource = modelSource;
		b.sourceType = sourceType;
		if (options != null) 
		    b.options = new NamedVal.NList(options);
		return b;
	    }
	}

	// Loops run 
	public static class Loops implements JobInfo {
	    public int[] counts; // inner=[0], outer=[1]
	    public NamedVal.NList baseVals; // base single run info
	    public String[] runNames; // names for each loop run
	    public NamedVal.NList[] nvals; // mod vals for each loop
	    public Loops() { }
	    public int n() { return runNames.length; }
	}
	
	// Serializable loops run
	public static class LoopsInfo implements Serializable {
	    public int[] counts; // outer=[0], inner=[1]
	    public NamedVal[] baseVals; // base single run info
	    public String[] runNames; // run names
	    public NamedVal[][] nvals; // mod vals for each loop
	    public LoopsInfo() { }
	    public LoopsInfo(Loops loops) throws Xcept {
		counts = loops.counts;
	        baseVals = loops.baseVals.info();
	    	runNames = loops.runNames;
		nvals = new NamedVal[loops.nvals.length][];
		for (int i=0; i<nvals.length; i++) 	
		    nvals[i] = loops.nvals[i].info();
	    }
	    public Loops loops() throws Xcept {
	    	Loops loops = new Loops();
		loops.counts = counts;
		loops.baseVals = new NamedVal.NList(baseVals);
		loops.runNames = runNames;
		loops.nvals = new NamedVal.NList[nvals.length];
		for (int i=0; i<nvals.length; i++) 
		    loops.nvals[i] = new NamedVal.NList(nvals[i]);
		return loops;
	    }
	}

	// Sensitivity run
	public static class Sens implements JobInfo {
	    public NamedVal.NList baseVals; // base single run info
	    public String[] parNames; // pars to vary
	    public double[] deltas; // deltas to vary by
	    public Sens(int n) { 
	    	parNames = new String[n];
		deltas = new double[n];
	    }
	}
	
	// Serializable Sensitivity run
	public static class SensInfo implements Serializable {
	    public NamedVal[] baseVals; // base single run info
	    public String[] parNames; // pars to vary
	    public double[] deltas; // deltas to vary by
	    public SensInfo() { }
	    public SensInfo(Sens sens) throws Xcept { 
	        baseVals = sens.baseVals.info();
	    	parNames = sens.parNames;
		deltas = sens.deltas;
	    }
	    public Sens sens() throws Xcept {
	    	Sens sens = new Sens(0);
		sens.baseVals = new NamedVal.NList(baseVals);
		sens.parNames = parNames;
		sens.deltas = deltas;
		return sens;
	    }
	}
	
	// Optimization run
	public static class Optim implements JobInfo {
	    public int[] loopCounts; // loop structure
	    public NamedVal.NList baseVals; // base single run info
	    public OptimArgs args; // optimizer arguments
	    public Data[] refData; // reference data
	    public String[] matchExprs; // model expr to match refData
	    public String[] pointWgts; // exprs to calc point wgts
	    public double[] curveWgts; // numeric curve wgts
	    public Optim() { }
	    public Optim(int npar, int nmatches) throws Xcept {
	        args = new OptimArgs(npar);
		refData = new Data[nmatches];
		matchExprs = new String[nmatches];
		pointWgts = new String[nmatches];
		curveWgts = new double[nmatches];
	    }
	    
	    // query
	    public int npars() { return args.nx(); }
	    public int nmatches() { return refData.length; }
	    public Optim copy() throws Xcept {
	    	OptimInfo info = new OptimInfo(this);
		return info.optim();
	    }

	    // data-to-match OptimResults enhancement
	    public void enhanceReport() throws Xcept {
	    	TextColumns cols = new TextColumns();
	    	cols = new TextColumns();
	    	cols.addColumn(10, "Curve");
	    	cols.addColumn(20, "Par/Expr");
	    	cols.addColumn(15, "Point Wgts");
	    	cols.addColumn(15, "Curve Wgt");
	    	cols.println();
	    	for (int i=0; i<matchExprs.length; i++) {
	            cols.print(refData[i].legend());
	            cols.print(matchExprs[i]);
	            cols.print(pointWgts[i]);
	            cols.print((float) curveWgts[i]);
		    cols.println();
	        }
	        args.matchReport = "Data to Match:\n" + cols;
	    }
	}

	// serializable Optimization run info
	public static class OptimInfo implements Serializable {
	    public NamedVal[] baseVals; // base single run info
	    public OptimArgs args; // optimizer arguments
	    public DataInfo[] refData; // reference data
	    public String[] matchExprs; // model expr to match refData
	    public String[] pointWgts; // exprs to calc point wgts
	    public double[] curveWgts; // numeric curve wgts
	    public OptimInfo() { }
 	    public OptimInfo(Optim optim) throws Xcept {
	        baseVals = optim.baseVals.info();
	    	args = optim.args;
		refData = new DataInfo[optim.refData.length];
		for (int i=0; i<refData.length; i++) 
		    refData[i] = optim.refData[i].info();
		matchExprs = optim.matchExprs;
		pointWgts = optim.pointWgts;
		curveWgts = optim.curveWgts;
	    }
	    public Optim optim() throws Xcept {
	    	Optim optim = new Optim(args.nx(), refData.length);
		optim.baseVals = new NamedVal.NList(baseVals);
		optim.args = args;
		optim.refData = new Data[refData.length];
		for (int i=0; i<refData.length; i++) 
		    optim.refData[i] = Data.makeData(refData[i]);
		optim.matchExprs = matchExprs;
		optim.pointWgts = pointWgts;
		optim.curveWgts = curveWgts;
		return optim;
	    }
	}
	
	// multiple optimization run
	public static class Mopt implements JobInfo {
	    public Optim optim;       // base optimization info
	    public NamedVal[][] nvals; // par changes for each optim
	    public Data[][] refData;  // ref data [segment][match]
	    public boolean saveOptimResults; // save optim results?
	    public boolean noabort;  // don't abort if optim fails
	    public Mopt() { } 

	    // query
	    public int nsegments() { return refData.length; }
	    public int npars() { return optim.npars(); }
	    public int nmatches() { return optim.nmatches(); }
	}

	// serializable info for multiple optimization run
	public static class MoptInfo implements Serializable {
	    public int nsegments;          // # optims to perform
	    public OptimInfo optim;        // base optimization info
	    public NamedVal[][] nvals; // par changes for each optim
	    public DataInfo[][] refData; // ref data for each optim
	    public boolean saveOptimResults; // save optim results?
	    public boolean noabort;  // don't abort if optim fails
	    public MoptInfo() { }
	    public MoptInfo(Mopt mopt) throws Xcept {
		nsegments = mopt.nsegments();
	        optim = new OptimInfo(mopt.optim);
		nvals = mopt.nvals;
		int nmatches = mopt.nmatches();
		refData = new DataInfo[nsegments][nmatches];
		for (int s=0; s<nsegments; s++) {
		    for (int m=0; m<nmatches; m++) {
		    	Data data = mopt.refData[s][m];
			if (data == null) continue;
			refData[s][m] = data.info();
		    }
		}
		saveOptimResults = mopt.saveOptimResults;
		noabort = mopt.noabort;
	    }
	    public Mopt mopt() throws Xcept {
	    	Mopt mopt = new Mopt();
		mopt.optim = optim.optim();
		mopt.nvals = nvals;
		int nmatches = mopt.optim.nmatches();
		mopt.refData = new Data[nsegments][nmatches];
		for (int s=0; s<nsegments; s++) {
		    for (int m=0; m<nmatches; m++) {
		    	DataInfo dinfo = refData[s][m];
			if (dinfo == null) continue;
			mopt.refData[s][m] = Data.makeData(dinfo);
		    }
		}
		mopt.saveOptimResults = saveOptimResults;
		mopt.noabort = noabort;	
		return mopt;	
	   }
		
		
	}

	// Job Status
	public static class Status implements Serializable {
	    public int id; // running job ID
	    public int mode; // see ASModel constants
	    public long startTime; // job start time
	    public long stopTime; // job stop time
	    public int nruns; 	// # "runs" within job
	    public int nrunsDone; // # "runs" completed
	    public int nrunsBest; // best run so far (mode=OPTIM only)
	    public String runName; // current "run" name
	    public RunStatus[] runStats; // current run status
	    public Status() { }
	    public Status(int n) {
	    	runStats = new RunStatus[n];
		try {
		    startTime = System.currentTimeMillis();
		} catch (Exception e) {
		}
	    }

	    public RunStatus runStat(int i) {
	    	if (runStats == null) return null;
		if (runStats.length <= i) return null;
		return runStats[i];
	    }
	}

	// Single run status
	public static class RunStatus implements Serializable {
	    public int phase; // curr phase# name within run
	    public double frac; // curr phase frac completed
	    public RunStatus() { }
	    public RunStatus(RunStatus r) {
	    	if (r == null) return;
		phase = r.phase;
		frac = r.frac;
	    }
	    public String toString() {
	    	return "phase=" + phase + " frac=" + frac;
	    }
	}
	
	// server message
	public static class Message implements Serializable {
	    public String modelID; // model ID, or null
	    public boolean warning; // is warning?
	    public String text;	// text of message
	    public Message() { }
	    public Message(String m, boolean w, String txt) {
	    	modelID = m;
		warning = w;
		text = txt;
	    }
	}
}

	    
