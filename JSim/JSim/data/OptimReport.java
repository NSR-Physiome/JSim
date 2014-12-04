/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// optimizer report

package JSim.data;

import java.io.Serializable;
import JSim.util.*;

public class OptimReport {
	private OptimResults res;
	private OptimArgs args;
	private OptimAlg alg;
	private int nx;
	private int prec;

	private static String[] SELECT_METHOD_NAMES = new String[] {
	    "roulette", "tournament", "elitism" };
	
	// constructor
	public OptimReport(OptimResults res, OptimAlg.NList algs) {
	    this(res, algs.alg(res.args.alg)); // HACK!!!
	}
	public OptimReport(OptimResults res, OptimAlg alg) {
	    this.res = res;
	    args = res.args;
	    nx = args.nx();
	    this.alg = alg;
	    prec = res.args.reportPrec;
	    if (prec < 3 || prec > 16) 
	    	prec = 4;
	}

	// create report
	public String getReport() {
	    if (alg == null) 
	        return "Unknown optimizer algorithm: " + args.alg;
	    StringBuffer s = new StringBuffer();
	    s.append(getArgsReport());

	    // termination status
	    s.append("\nOptimization Results:\n");
	    s.append("   termination status   " + res.termMsg + "\n");
	    s.append("   # evaluations        " + res.nCalls + "\n");
	    s.append("   best RMS error       " + 
	        PrettyFormat.sformat(res.bestErr, prec) + 
		" on run #" + (res.bestCall+1) + "\n");

	    // initialize param values & confidence limits table
	    boolean doSD = res.covMat != null;
	    boolean doConf = args.confPcts != null 
	        && res.confLims != null;
	    int npcts = doConf ? 
	        Math.min(args.confPcts.length, res.confLims.length) : 0;
	    TextColumns cols = new TextColumns();
	    int precWidth = prec + 6;
	    int[] parWidths = new int[nx];
	    int parMaxWidth = 0;
	    for (int x=0; x<nx; x++) {
	    	int w = args.xname[x].length() + 2;
		w = Math.max(w, precWidth);
		parWidths[x] = w;
		parMaxWidth = Math.max(parMaxWidth, w);
	    }
	    int floatWidth = 15;

	    // header 
	    cols.addColumn(parMaxWidth, "Parm");
	    cols.addColumn(floatWidth, "value");
	    if (doSD)
	    	cols.addColumn(precWidth, "+/- 1SD");
	    for (int i=0; i<npcts; i++) {
		int p = (int) (100 * args.confPcts[i]);
		cols.addColumn(precWidth, "" + p + "%");
	    }
 	    cols.println();
		
	    // parameter lines		
	    for (int i=0; i<nx; i++) {
	        cols.print(args.xname[i]);
		cols.print(res.bestX[i]);
		if (doSD) {
		    double sd = Math.sqrt(res.covMat[i][i]);
		    cols.print(sd, prec);
		}
		for (int j=0; j<npcts; j++) 
		    cols.print(res.confLims[j][i], prec);
		cols.println();
	    }
	    s.append("   best parameter values:");
	    s.append("\n" + cols);

	    // covariance matrix
	    if (nx>1 && res.covMat != null) {
	    	s.append("\nCorrelation matrix:" +
		    " (condition number=" + 
		    PrettyFormat.sformat(res.condno, prec) + ")");
	    	cols = new TextColumns();
	    	cols.addColumn(parMaxWidth, ""); 
	    	for (int i=0; i<nx; i++) 
	    	    cols.addColumn(parWidths[i], args.xname[i]);
		cols.println();
		double[][] normMat = SensMatrix.normalize(res.covMat);
	    	for (int i=0; i<nx; i++) {
	    	    cols.print(args.xname[i]);
		    for (int j=0; j<nx; j++) 
		    	cols.print(normMat[i][j], prec);
		    cols.println();
	    	}
	    	s.append("\n" + cols);
	    }
	    
	    // optional log
	    if (res.logErr != null) {
		cols = new TextColumns();
		cols.addColumn(precWidth, "run#");
		for (int i=0; i<nx; i++) 
		     cols.addColumn(parWidths[i], args.xname[i]);
		cols.addColumn(precWidth, "RMS error");
		cols.println();
		int ct = Math.min(res.nCalls, res.logErr.nsamples());
	    	for (int i=0; i<ct; i++) {
		    cols.print(i+1);
		    for (int j=0; j<nx; j++) {
			RealNData xdata = (RealNData) res.logX.data(j);
			cols.print(xdata.realVal(i), prec);
		    }
		    cols.print(res.logErr.realVal(i), prec);
		    cols.println();
		}
		s.append("\n" + cols);
	    }
	    
	    // return
	    return s.toString();
	}

	// create OptimArgs portion of report
	public String getArgsReport()  {
	    StringBuffer s = new StringBuffer();
	    s.append("Optimizer Tuning Parameters:\n");
	    s.append("    Algorithm       " + alg.name() + "\n");
	    s.append("    Max # runs      " + args.maxCalls + "\n");
	    s.append("    Min RMS error   " + args.errTol + "\n");
	    s.append("    Min par step    " + args.stepTol + "\n");
	    if (alg.parNeeded("npoints")) 
	    	s.append("    # grid points   " + args.npoints + "\n");
	    if (alg.parNeeded("maxIters")) 
	    	s.append("    Max # iter      " + args.maxIters + "\n");
	    if (alg.parNeeded("gradTol")) 
	    	s.append("    Min gradient    " + args.gradTol + "\n");
	    if (alg.parNeeded("eps")) 
	    	s.append("    Relative error  " + args.eps + "\n");
	    if (alg.parNeeded("randomSeed")) 
	    	s.append("    Random seed     " + args.randomSeed + "\n");
            if (alg.parNeeded("selectMethod")) {
	    	String sm = (args.selectMethod >=0 && 
		    args.selectMethod < SELECT_METHOD_NAMES.length) ?
		    SELECT_METHOD_NAMES[args.selectMethod]
		    : ("" + args.selectMethod);
                s.append("    Select Method   "  + sm + "\n");
	    }
	    if (alg.parNeeded("populationSize")) 
	    	s.append("    Population size " + args.populationSize + "\n");
	    if (alg.parNeeded("mutationRate")) 
	    	s.append("    Mutation rate   " + args.mutationRate + "\n");
	    if (alg.parNeeded("crossoverRate")) 
	    	s.append("    Crossover rate  " + args.crossoverRate + "\n");
	    if (alg.parNeeded("mutationStep")) 
	    	s.append("    Mutation step   " + args.mutationStep + "\n");
	    if (alg.parNeeded("eliteCutoff")) 
	    	s.append("    Elite Cutoff    " + args.eliteCutoff + "\n");

	    s.append("\nParameters to Vary:\n");
	    TextColumns cols = new TextColumns();
	    cols.addColumn(15, "Parameter");
	    cols.addColumn(15, "Start");
	    if (alg.boundsNeeded()) {
	    	cols.addColumn(15, "Min");
	    	cols.addColumn(15, "Max");
	    }
	    if (alg.parNeeded("xstep")) 
	    	cols.addColumn(15, "Step");
	    cols.println();
	    for (int i=0; i<nx; i++) {
		cols.print(args.xname[i]);
		cols.print((float) args.xstart[i]);
	    	if (alg.boundsNeeded()) {
		    cols.print((float) args.xmin[i]);
		    cols.print((float) args.xmax[i]);
		}
	    	if (alg.parNeeded("xstep")) 
		    cols.print((float) args.xistep[i]);
		cols.println();
	    }
	    s.append(cols.toString());

	    if (args.matchReport != null) 
		s.append("\n" + args.matchReport);

	    return s.toString(); 
	}

}
