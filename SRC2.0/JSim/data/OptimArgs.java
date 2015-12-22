/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// arguments to general purpose optimizer

package JSim.data;

import java.io.Serializable;
import JSim.util.*;

public class OptimArgs implements Serializable {

	// general state
	public String alg; // algorithm name
	private int nx;	 // # pars to optimize
	public String[] xname; // user name for x[i]
	public double[] xstart; // starting x vals, or NaNs
	public double[] xmin; // min x vals, or NaNs 
	public double[] xmax; // max x vals, or NaNs
	public int maxCalls; // max # calls (rough)
	public double errTol; // min error value tolerance 
	public double stepTol; // relative step tolerance
	public boolean saveLogs; // save x & err value logs?
	public String matchReport; // desc of data match criteria(if any)
	public int reportPrec; // output precision in report
	public boolean calcCovMat; // calc covariance matrix?
	public double[] confPcts; // confidence fracs (.95=95%)
	public int maxIters; // maximum # iterations 
	public int maxStaticIters; // max # iters w/ no improvement
	public long randomSeed; // <= 0 implies timer seeded
	
	// simplex specific
	public double[] xistep; // initial absolute step sizes

	// ggopt specific
	public double gradTol; // minimum gradient tolerance 
	public double eps; // relative func error (1e-7 guess???) 

	// GridSearch specific
	public int npoints; // # grid points each dimension
	
	// SimAnneal specific
	public double initTemp; // initial "temperature" (must be >0)

	// Genetic algorithm specific
	public int populationSize;  
	public double mutationRate; // 0-1
	public double crossoverRate; // 0-1
	public double mutationStep;
        public int selectMethod;
        public double eliteCutoff;

	// constructors 
	public OptimArgs() { } // RMI constructor
	public OptimArgs(String alg, int n) {
	    this(n);
	    this.alg = alg;
	}
	public OptimArgs(int n) {
	    alg = "";
	    nx = n;
	    xname = new String[nx];
	    xstart = new double[nx];
	    xmin = new double[nx];
	    xmax = new double[nx];
	    xistep = new double[nx];
	    for (int i=0; i<nx; i++) {
		xstart[i] = xmin[i] = xmax[i] = 
		xistep[i] = Double.NaN;
		xname[i] = "x" + i;
	    }
	    matchReport = null;
	    calcCovMat = false;
	    confPcts = null;
	}

	// copy constructor
	public OptimArgs(OptimArgs a) throws Xcept {
	    this(a.alg, a.nx);
	    for (int i=0; i<nx; i++) {
	    	xname[i] = a.xname[i];
		xstart[i] = a.xstart[i];
		xmin[i] = a.xmin[i];
		xmax[i] = a.xmax[i];
		xistep[i] = a.xistep[i];
	    }
	    maxCalls = a.maxCalls;
	    errTol = a.errTol;
	    stepTol = a.stepTol;
	    matchReport = a.matchReport;
	    reportPrec = a.reportPrec;
	    calcCovMat = a.calcCovMat;
	    if (a.confPcts != null) 
	    	confPcts = (double[]) a.confPcts.clone();		
	    maxIters = a.maxIters;
	    maxStaticIters = a.maxStaticIters;
	    gradTol = a.gradTol;
	    eps = a.eps;
	    npoints = a.npoints;
	    mutationRate = a.mutationRate;
	    populationSize = a.populationSize;
	    crossoverRate = a.crossoverRate;
	    mutationStep  = a.mutationStep;
	    selectMethod  = a.selectMethod;
	    eliteCutoff   = a.eliteCutoff;
	}

	// query
	public int nx() { return nx; }
	public String alg() { return alg; }

}
