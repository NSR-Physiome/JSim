/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// results from general purpose optimizer

package JSim.data;

import JSim.util.*;
import java.util.ArrayList;
import java.io.Serializable;

public class OptimResults {
	// status values
	public final static int NORMAL = 1; // normal completion
 	public final static int CANCEL = 2; // user cancelled
 	public final static int ERROR = 3; // error (see termMsg)
 
	// instance fields
	public OptimArgs args; // arguments to optimizer
	public long lastUpdated;// time of last update (msecs)
	public int nCalls;     // # calls made
	public int bestCall;   // which call was best (lowest err)
	public double[] bestX; // best x vals, or NaNs
	public double bestErr; // best (lowest) error value
	public DataCompare bestCompare; // optional
	public int status;     // completion status (above)
	public String termMsg; // termination message
	public Data.List logX; // log of x vals
	public RealNData logErr; // log of error values
	public double[][] covMat;  // covariance matrix
	public double[][] confLims; // conf limits [npcts][nx]
	public double condno;  // covMat condition number
	
	// simplex specific
	public double finalStep; // final step size

	// standard constructor
	public OptimResults(OptimArgs a) throws Xcept {
	    args = a;
	    bestX = new double[nx()];
	    for (int i=0; i<nx(); i++) 
		bestX[i] = Double.NaN;
	    bestErr = Double.NaN;
	    if (args.saveLogs) {
		int n = args.maxCalls + 1; // some spillover room
		GridData logN = new RegularGridData("run#", null, 1, n, n);
		GridData[] grids = new GridData[] { logN };
		logX = new Data.List(nx());
		for (int i=0; i<nx(); i++) 
		    logX.add(new RealNData(
			args.xname[i], null, grids));
		logErr = new RealNData("RMS_error", null, grids);
	    }
	}

	// serialized constructor
	public OptimResults(Info info) throws Xcept {
	    this(info.args);
	    importInfo(info);
	}

	// add result of last call
	public synchronized void addResult(double err, double[] x) 
	throws Xcept {
	    if (args.saveLogs) addLogs(nCalls, x, err);
	    nCalls++;
	    lastUpdated = System.currentTimeMillis();
	    if (! newBest(err)) return;
	    bestCall = nCalls-1;
	    bestErr = err;
	    for (int i=0; i<x.length; i++) 
		bestX[i] = x[i];
	}

	// add x, err data to logs
	private void addLogs(int n, double[] x, double err) {
	    if (n>=logErr.samples().length) return;
	    for (int i=0; i<nx(); i++) {
		RealNData xdata = (RealNData) logX.data(i);
		xdata.set(n, x[i]);
	    }
	    logErr.set(n, err);
	}
	    
	// is error value a new best (lowest) value?
	public boolean newBest(double err) {
	    if (Double.isNaN(err)) return false;
	    if (Double.isNaN(bestErr)) return true;
	    return err < bestErr;
	}

	// best update data compare
	public void bestUpdate(DataCompare dc) {
	    bestCompare = dc;
	}

	// query
	public Info info() throws Xcept { return new Info(this); }
	public int nx() { return args.nx(); }
	public RealNData logErr() { return logErr; }
	public RealNData logX(int n, boolean normalized) throws Xcept {
	    if (logX == null) return null;
	    RealNData xdata = (RealNData) logX.data(n);
	    if (!normalized) return xdata;
	    GridData[] grids = new GridData[] { xdata.grid(0) };
	    RealNData ndata = new RealNData(xdata.legend(),
		xdata.unit, grids);
	    double min = args.xmin[n];
	    double max = args.xmax[n];
	    if (min >= max) {
		min = min - 0.5;
		max = min + 1;
	    }
	    int ct = Math.min(nCalls, xdata.nsamples());
	    for (int i=0; i<ct; i++) {
		double x = (xdata.realVal(i) - min) / (max - min);
		ndata.set(i, x);
	    }
	    return ndata;
	}
	public Data.List logX(boolean normalized) throws Xcept {
	    if (logX == null) return null;
	    if (! normalized) return logX;
	    Data.List list = new Data.List(logX.size());
	    for (int i=0; i<logX.size(); i++) 
		list.add(logX(i, normalized));
	    return list;
	}

	// import serialized info
	public void importInfo(Info info) throws Xcept {
	    lastUpdated = info.lastUpdated;
	    nCalls = info.nCalls;
	    bestCall = info.bestCall;
	    bestErr = info.bestErr;
	    bestX = info.bestX;
	    if (info.bestCompare != null) 
	    	bestCompare = new DataCompare(info.bestCompare);
	    status = info.status;
	    termMsg = info.termMsg;
	    logX = new Data.List(info.logX);
	    logErr = (RealNData) Data.makeData(info.logErr);
	    finalStep = info.finalStep;
	    covMat = info.covMat;
	    confLims = info.confLims;
	    condno = info.condno;
	}

	// serializable info
	public static class Info implements Serializable {
	    public OptimArgs args;  // optim args	
	    public long lastUpdated;// time of last update (msecs)
	    public int nCalls;      // # calls made
	    public int bestCall;    // which call was best
	    public double[] bestX;  // best x vals, or NaNs
	    public double bestErr;  // best error value
	    public DataCompare.Info bestCompare; // optional
	    public int status;      // completion status (above)
	    public String termMsg;  // termination message
	    public DataInfo[] logX; // log of x vals
	    public DataInfo logErr; // log of error values
	    public double finalStep; // final step size
	    public double[][] covMat; // covariance matrix
	    public double[][] confLims; // conf limits
	    public double condno;  // condition number from covmat

	    // constructor
	    public Info(OptimResults r) throws Xcept {
		args = r.args;
	        lastUpdated = r.lastUpdated;
	        nCalls = r.nCalls;
	        bestCall = r.bestCall;
		bestErr = r.bestErr;
	        bestX = r.bestX;
		if (r.bestCompare != null) 
	            bestCompare = r.bestCompare.info();
	        status = r.status;
	        termMsg = r.termMsg;
	        logX = r.logX.info();
	        logErr = r.logErr.info();
	        finalStep = r.finalStep;
		covMat = r.covMat;
		confLims = r.confLims;
		condno = r.condno;
	    }
	}
}
