/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Optimization configuration for a model

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;
import java.util.*;

public class PModelOptim extends PNamed {
	
	// controls
	public IntControl npars; // # Par's 
	public IntControl nmatches; // # Match's 
	public StringControl alg; // optimizer algorithm to use
	public IntControl maxCalls; // max # calls
	public IntControl maxIters; // max # iterations (ggopt)
	public RealControl errTol; // RMS error tolerance
	public RealControl stepTol; // par step tolerance
	public RealControl gradTol; // gradient tolerance
	public IntControl npoints; // # points for GridSearch
	public RealControl eps; // relative error
	public IntControl randomSeed; // random seed, if > 0
	public RealControl initTemp; // init temperature
	public IntControl populationSize; // GA population
	public RealControl mutationRate; // GA mutations 0-1
	public RealControl crossoverRate; // GA crossover 0-1
	public RealControl mutationStep;
        public ChoiceControl selectMethod;
        public RealControl eliteCutoff;

	public BooleanControl calcCovMat; // calc covMat after opt?
	public IntControl reportPrec; // report result numeric precision

	// private instance fields
	private Par.List pars; // list of Par's
	private Match.List matches; // list of Match's
	private ArrayList<PModelOptimGraph> graphs; // graphs

	// constructor
	public PModelOptim(PModel p, String n) throws Xcept {
	    super(p, n);
	    pars = new Par.List(2);
	    matches = new Match.List(2);
	    graphs = new ArrayList<PModelOptimGraph>(PModelOptimGraph.NGRAPHS);
	    npars = new IntControl(this, "npars", 1) {
		public void updateOther() throws Xcept {
		    reconfigPars();
		}
	    };
	    nmatches = new IntControl(this, "nmatches", 1) {
		public void updateOther() throws Xcept {
		    reconfigMatchs();
		}
	    };
	    alg = new StringControl(this, "alg", "simplex",
		server().optimAlgs().getNames(OptimAlg.ALL));
	    maxCalls = new IntControl(this, "maxCalls", 50);
	    maxIters = new IntControl(this, "maxIters", 10);
	    errTol = new RealControl(this, "errTol", 1e-3);
	    stepTol = new RealControl(this, "stepTol", 1e-6);
	    gradTol = new RealControl(this, "gradTol", 1e-6);
	    npoints = new IntControl(this, "npoints", 5);
	    eps = new RealControl(this, "eps", 1e-6);
	    randomSeed = new IntControl(this, "randomSeed", 0);
	    initTemp = new RealControl(this, "initTemp", 100);
	    populationSize = new IntControl(this, "populationSize", 25);
	    mutationRate = new RealControl(this, "mutationRate", 0.1);
	    crossoverRate = new RealControl(this, "crossoverRate", 0.5);
            mutationStep = new RealControl(this, "mutationStep", 0.05);
            selectMethod = new ChoiceControl(this, "selectMethod",  0,
	    	new String[] { "roulette", "tournament", "elitism" });
            eliteCutoff = new RealControl(this, "eliteCutoff", 0.5);
	    calcCovMat = new BooleanControl(this, "calcCovMat", true);
	    reportPrec = new IntControl(this, "reportPrec", 4);
	    for (int i=0; i<PModelOptimGraph.NGRAPHS; i++) 
	    	graphs.add(new PModelOptimGraph(this,
		    PModelOptimGraph.NAMES[i]));
	    reconfigPars();
	    reconfigMatchs();
	}

	// change npars
	public void reconfigPars() throws Xcept {
	    int ct = npars.val();
	    if (ct<1) ct=1;
	    int ct0 = pars.size();

	    // add missing children
	    for (int i=ct0; i<ct; i++) 
		pars.add(new Par(this, "optpar" + i));

	    // remove extra children
	    for (int i=ct0-1; i>=ct; i++) {
		Par par = par(i);
		int inx = pars.indexOf(par);
		if (inx <= 0) throw new Xcept(
		    "Internal error removing optpar " + i);
		pars.remove(par);
		remove(par);
	    }
	}
	    
	// change nmatches
	public void reconfigMatchs() throws Xcept {
	    int ct = nmatches.val();
	    if (ct<1) ct=1;
	    int ct0 = matches.size();

	    // add missing children
	    for (int i=ct0; i<ct; i++) 
		matches.add(new Match(this, "optmatch" + i));

	    // remove extra children
	    for (int i=ct0-1; i>=ct; i++) {
		Match match = match(i);
		int inx = matches.indexOf(match);
		if (inx <= 0) throw new Xcept(
		    "Internal error removing optmatch " + i);
		matches.remove(match);
		remove(match);
	    }
	}
	    
	// collect optimization job info
	public ASInfo.Optim makeJobInfo() throws Xcept {

	    // collect runnable pars
	    Par.List runPars = new Par.List(4);
	    for (int i=0; i<pars.size(); i++) {
	    	Par par = par(i);
		if (! par.enabled()) continue;
		if (! par.valid()) continue;
		runPars.add(par);
	    }
	    if (runPars.size()<1) throw new Xcept(this,
	    	"no valid parameters to vary");
		
	    // collect runnable matches
	    Match.List runMatches = new Match.List(4);
	    for (int i=0; i<matches.size(); i++) {
	        Match match = match(i);
		if (! match.enabled()) continue;
		if (! match.valid()) continue;
		runMatches.add(match);
	    }
	    if (runMatches.size()<1) throw new Xcept(this,
	    	"no valid data to match");

	    // initialize job info
	    ASInfo.Optim jinfo = new ASInfo.Optim(
	    	runPars.size(), runMatches.size());
	    jinfo.baseVals = pmodel().makeJobInfo();
	    
	    OptimArgs args = jinfo.args;
	    args.alg = alg.val();
	    args.maxCalls = maxCalls.val();
	    args.maxIters = maxIters.val();
	    args.errTol = errTol.val();
	    args.saveLogs = true;
	    args.stepTol = stepTol.val();
	    args.gradTol = gradTol.val();
	    args.eps = eps.val();
	    args.npoints = npoints.val();
	    args.randomSeed = randomSeed.val();
	    args.initTemp = initTemp.val();
	    args.populationSize = populationSize.val();
	    args.mutationRate = mutationRate.val();
	    args.crossoverRate = crossoverRate.val();
            args.mutationStep = mutationStep.val();
            args.selectMethod = selectMethod.val();
            args.eliteCutoff  = eliteCutoff.val();
	    args.reportPrec = reportPrec.val();
	    
	    args.calcCovMat = calcCovMat.val();
	    if (args.calcCovMat) 
	    	args.confPcts = new double[] { .9, .95, .99 };

	    // add pars-to-vary info
	    for (int i=0; i<runPars.size(); i++) {
		Par par = (Par) runPars.pnamed(i);
		Control cntl = par.par.control();
		args.xname[i] = par.par.stringVal();
		args.xstart[i] = cntl.realVal();
		if (Double.isNaN(args.xstart[i])) 
		    throw new Xcept(cntl,
		    "illegal optimizer starting value");
		args.xmin[i] = par.min.realVal();
		args.xmax[i] = par.max.realVal();
		args.xistep[i] = par.step.realVal();
		if (optimAlg().boundsNeeded()) {
		    if (args.xmax[i] < args.xstart[i])
			throw new Xcept(this,
			"Optimization parameter \"" + cntl.name() + 
			"\" maximum less than start value");
		    if (args.xmin[i] > args.xstart[i])
			throw new Xcept(this,
			"Optimization parameter \"" + cntl.name() + 
			"\" minimum greater than start value");
		}
	    }

	    // add data-to-match info
	    for (int i=0; i<runMatches.size(); i++) {
	    	Match match = (Match) runMatches.pnamed(i);
		jinfo.refData[i] = match.data.getData(0);
		jinfo.matchExprs[i] = match.expr.getExpr().toString();
		jinfo.pointWgts[i] = 
		    match.pointWgts.getExpr().toString();
		jinfo.curveWgts[i] = match.curveWgt.val();
	    }

	    // enhance data to match report & return
	    jinfo.enhanceReport();
	    return jinfo;
	}		
	    
	// query
	public Par par(int i) { 
	    if (i<0 || i>=pars.size()) return null;
	    return (Par) pars.get(i);
	}
	public Match match(int i) { 
	    if (i<0 || i>=matches.size()) return null;
	    return (Match) matches.get(i);
	}
	public PModelOptimGraph graph(int i) {
	    return graphs.get(i);
	}
	public String diagInfo() {
	    return "Model Optimization Config " + pmodel().name();
	}
	public String xmlLabel() { return "optim"; }
	public OptimAlg optimAlg() throws Xcept {
	    OptimAlg optimAlg = server().optimAlgs().alg(alg.val());
	    if (optimAlg == null) throw new Xcept(
	    	alg.val() + ": Unknown optimization algorithm");
	    return optimAlg;
	}

	// PModelOptim.Par class
	public static class Par extends PValidGroup {
	    public ModelParControl par;
	    public RealControl min; // par min value
	    public RealControl max; // par max value
	    public RealControl step; // par step (simplex)
	    public BooleanControl enabled; // this par enabled
	
	    // constructor
	    public Par(PModelOptim p, String n) throws Xcept {
		super(p, n);
		par = new ModelParControl(this, "par", pmodel());
		min = new RealControl(this, "min", Double.NaN);
		max = new RealControl(this, "max", Double.NaN);
		step = new RealControl(this, "step", 0.01);
		enabled = new BooleanControl(this, "enabled", true);
	    }

	    // query
	    public String diagInfo() { 
		return "optimization par " + name();
	    }
	    public String xmlLabel() { return "optpar"; }
	    public BooleanControl enableControl() { return enabled; }
	    public boolean isBlank() { return par.isBlank(); }

	    // valid message
	    public String validMsg() {
		if (par.isBlank()) return "parameter missing";
		if (! par.valid()) return par.validMsg();
		OptimAlg alg;		
		try {
		    alg = ((PModelOptim) parent()).optimAlg();
		} catch (Xcept e) {
		    return e.toString();
		}
		if (alg.boundsNeeded()) {
		    if (Double.isNaN(min.val())) return "minimum missing"; 
		    if (Double.isNaN(max.val())) return "maximum missing"; 
		}
		if (alg.parNeeded("xstep") && Double.isNaN(step.val())) 
		    return "step missing"; 
		return null;
	    }
	}

	// PModelOptim.Match class
	public static class Match extends PValidGroup {
	    public PNamedControl dataSrc; // which dataset
	    public DataControl data; // dataset curve
	    public DataControl expr; // model data expression
	    public DataControl pointWgts; // point weights expr
	    public RealControl curveWgt; // curve weight
	    public BooleanControl enabled; // this matchs enabled
	
	    // constructor
	    public Match(PModelOptim p, String n) throws Xcept {
		super(p, n);
	    	dataSrc = new PNamedControl(this, "src", project(), 
		    new Class[] { PDataSet.class });
		data = new DataControl(this, "data", dataSrc);
		expr = new DataControl(this, "expr", pmodel());
		pointWgts = new DataControl(this, "pointWgts", 
		    pmodel(), "1");
		curveWgt = new RealControl(this, "step", 1);
		enabled = new BooleanControl(this, "enabled", true);
	    }

	    // query
	    public String diagInfo() { 
		return "optimization match " + name();
	    }
	    public String xmlLabel() { return "optmatch"; }
	    public BooleanControl enableControl() { return enabled; }
	    public boolean isBlank() {
		return data.isBlank() && expr.isBlank();
	    }

	    // validation message
	    public String validMsg() {
		if (dataSrc.isBlank() && dataSrc.singlePick() == null)
		     return "data set missing";
		if (! dataSrc.valid()) return dataSrc.validMsg();
		if (data.isBlank()) return "data missing"; 
		if (! data.valid()) return data.validMsg(); 
		if (expr.isBlank()) return "par/expr missing"; 
		if (! expr.valid()) return expr.validMsg(); 
		if (pointWgts.isBlank()) return "point weights missing"; 
		if (! pointWgts.valid()) return pointWgts.validMsg(); 
		return null;
	    }
	}

	// is empty? determines whether saveable in ParSet
	public boolean isEmpty() {
	    if (pars == null) return true;
	    for (int i=0; i<pars.size(); i++)
	    	if (! ((Par) pars.get(i)).par.isBlank()) return false;
	    for (int i=0; i<matches.size(); i++) {
	    	if (! ((Match) matches.get(i)).data.isBlank()) return false;
	    	if (! ((Match) matches.get(i)).expr.isBlank()) return false;
	    }
	    return true;
	} 
	
	// control changed: update parset
	public void childChanged(PNamed c) throws Xcept {
	    super.childChanged(c);
	    if (! isEmpty() || pmodel().isEmptyControl(c))
	    	pmodel().setParsModified(true);
	}

}

