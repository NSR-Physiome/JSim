/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// monte-carlo report

package JSim.project;

import JSim.util.*;
import JSim.data.*;

public class PModelMonteReport {
	private PModelMonte pmonte;
	private MoptData moptData;
	private StringBuffer buf;
	private int prec;
	private int parWidth;
	private int precWidth;

	// constructor
	public PModelMonteReport(PModelMonte pmonte, MoptData moptData) {
	    this.pmonte = pmonte;
	    this.moptData = moptData;
	    setPrec(4);
	    parWidth = 12;
	    for (int i=0; i<npars(); i++) 
	    	parWidth = Math.max(parWidth, parName(i).length() + 2);
	}

	// set precision
	public void setPrec(int p) {
	    prec = p;
	    precWidth = prec + 8;
	}
	
	// create report
	public String getReport() throws Xcept {
	    buf = new StringBuffer();
	    println("Monte-Carlo Report");
	    println("");

	    // monte-carlo config
	    println("Configuration:");
	    print("# optimizations", pmonte.noptims);
	    print("random seed", pmonte.randomSeed);
	    print("distribution", pmonte.dist);
	    print("magnitude", pmonte.magnitude);
	    print("add method", pmonte.addMethod);
	    println("");
	    
	    // optimization config
	    OptimArgs oargs = pmonte.optimArgs();
	    if (oargs != null) { 
	    	OptimResults ores = new OptimResults(oargs);
	    	OptimReport orpt = new OptimReport(ores,
		    pmonte.server().optimAlgs());
		String s = orpt.getArgsReport();
		println(s);
	    }

	    // parameter stats
	    println("Parameter statistics:");
	    TextColumns cols = new TextColumns();
	    cols.addColumn(parWidth, "parameter");
	    cols.addColumn(precWidth, "mean");
	    cols.addColumn(precWidth, "sd");
	    cols.println();
	    for (int i=0; i<npars(); i++) {
		RealNData data = moptData.parData(i);
		double mean = DataStats.mean(data);
		double sd = DataStats.sd(data, mean);
		cols.print(parName(i));
		cols.print(mean, prec);
		cols.print(sd, prec);
		cols.println();
	    }
	    cols.println();
	    buf.append(cols.toString());

	    // covariance matrix
	    if (npars() > 1) 
	    	printCovMat();

	    // parameter estimates for each run
	    println("Parameter estimates:");
	    cols = new TextColumns();
	    cols.addColumn(8, "opt#");
	    for (int i=0; i<npars(); i++) 
	    	cols.addColumn(precWidth, parName(i));
	    cols.println();
	    for (int i=0; i<nsegments(); i++) {
	    	cols.print(i+1);
		for (int j=0; j<npars(); j++) {
		    RealNData pdata = moptData.parData(j);
		    cols.print(pdata.realVal(i), prec);
		}
		cols.println();
	    }
	    cols.println();
	    buf.append(cols.toString());		    

	    
	    return buf.toString();
	}

	// print covariance matrix
	private void printCovMat() throws Xcept {
	    double[][] covmat = new double[npars()][npars()];
	    for (int i=0; i<npars(); i++) {
		RealNData x = moptData.parData(i);
	    	for (int j=0; j<=i; j++) {
		    RealNData y = moptData.parData(j);
		    double cov = DataStats.covar(x, y);
		    covmat[i][j] = cov;
		    covmat[j][i] = cov;
		}
	    }
	    buf.append("Normalized Covariance Matrix:\n");
	    TextColumns cols = new TextColumns();
	    cols.addColumn(parWidth, ""); 
	    for (int i=0; i<npars(); i++) 
	    	cols.addColumn(parWidth, parName(i));
	    cols.println();
	    for (int i=0; i<npars(); i++) {
	    	cols.print(parName(i));
		for (int j=0; j<npars(); j++) 
		    cols.print(covmat[i][j], prec);
		cols.println();
	    }
	    cols.println();
	    buf.append(cols.toString());
	}

	// simple stuff
	private int npars() { return moptData.npars(); }
	private int nsegments() { return moptData.nsegments(); }
	private String parName(int i) { return moptData.parNames()[i]; }
	private void print(String s) { buf.append(s); }
	private void println(String s) { buf.append(s + "\n"); }

	// print control
	private void print(String s, Control c) {
	    s = "    " + s + ":";
	    while (s.length() < 22) s = s + " ";
	    String val = pmonte.args().get(c.name());
	    println(s + val);
	}
}


