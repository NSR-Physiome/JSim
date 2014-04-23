/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Local Server model optimizer

package JSim.lserver;

import JSim.util.*;
import JSim.jruntime.*;
import JSim.aserver.*; 
import JSim.data.*;
import JSim.nml.opt.*;

public class LSModelOptim implements OptimCallbacks, MPDispatch.Monitor {
	private LSModel lsmodel; // for this model
	private ASInfo.Optim jobInfo; // info for this job
	private OptimArgs args; // optimizer arguments
	private int nx;		// # parms in optimization
	private Optimizer opt; // this is the optimizer
	private OptimResults results; // results so far
	private ASQuery[] matchExprs; // model expr to match data
	private DataCompare compare; // to compare model & ref data
	private int threadInxBase; // thread/storeInx for "best" run
	private int nproc;  // # processors for this optim run
	private RTContext optimCtxt; // optimizer context
		// holds threadInx for native optim methods (if any)
	
	// constructor
	public LSModelOptim(LSModel m, ASInfo.Optim jinfo, 
	int threadInxBase, int np) throws Xcept {
	    lsmodel = m;
	    jobInfo = jinfo;
	    args = jobInfo.args;
	    nx = args.nx();
	    this.threadInxBase = threadInxBase;
	    opt = lsmodel.server().optimFactory().createOptimizer(args.alg()); 
	    results = new OptimResults(args);
	    nproc = (np == 0) ? maxProc() : np;
	    optimCtxt = new RTContext(lsmodel.rtmodel(),
	    	threadInxBase, nproc, null, false);

	    // parse data-matching model expressions
	    matchExprs = new ASQuery[nmatches()];
	    for (int i=0; i<nmatches(); i++) 
	    	matchExprs[i] = 
		    lsmodel.parseQuery(jobInfo.matchExprs[i]);
	}
	
	// run optimizer
	public void optimize() throws Xcept {
	    opt.optimize(optimCtxt, results, this);
	    if (! args.calcCovMat) return;

	    // calculate covariance mat, conf limits
	    double[] errs = new double[nx+1];
	    SensMatrix mat = calcSensMat(
	    	results.bestX, args.xistep, errs, false);	    
	    results.covMat = mat.getCovMatrix();
	    results.condno = mat.getCondNo();
	    if (args.confPcts == null) return; 
	    results.confLims = 
	    	new double[args.confPcts.length][nx];
	    for (int i=0; i<args.confPcts.length; i++) 
	    	results.confLims[i] = 
		    mat.getConfLimits(args.confPcts[i]);
	}

	//// public Optimizer callbacks
	
	// calc residual error from single run 
	public double calcError(RTContext ctxt, double[] x, 
	OptimResults res) throws Xcept {
	    if (x.length != nx) throw new Xcept(lsmodel,
	    	"calcError: array length inconsistent");
	    return singleRun(threadInxWorkBase(), x, 
	    	-1, null, true);
	}

	// calc error from multiple runs
	public int calcErrors(RTContext ctxt, double[][] x,
	double[] errs, OptimResults res) throws Xcept {
	    if (x.length != errs.length) throw new Xcept(lsmodel,
	    	"calcErrors: array lengths inconsistent");
	    return multRuns(x, errs, null, true);
	}

	// calc (errs!!!) &  sensmat for multiple runs
	public SensMatrix calcSensMatrix(RTContext ctxt, 
	double[] x, double[] dx, double[] errs, OptimResults res) throws Xcept {
	    if (x.length != nx || dx.length != nx ||
	    errs.length != (nx+1)) throw new Xcept(lsmodel,
	    	"calcSensMatrix: array lengths inconsistent");
	    return calcSensMat(x, dx, errs, true);
	}

	//// local SensMatrix support

	// calc sens matrix, using nx+1 model runs
	private SensMatrix calcSensMat(double[] xbase, double[] dx, 
	double[] errs, boolean updateResults) throws Xcept {
	    for (int i=0; i<dx.length; i++) 
	        if (dx[i] == 0) throw new Xcept(
		    "Zero parameter step size illegal in covariance matrix calculation");
	    double[][] x = new double[nx+1][nx];
	    for (int i=0; i<nx+1; i++) {
	    	for (int j=0; j<nx; j++) 
		    x[i][j] = xbase[j];
		if (i>0)
		    x[i][i-1] += dx[i-1];    
	    }
	    Data.List[] sensH = new Data.List[nx+1];
	    multRuns(x, errs, sensH, updateResults);
	    Data.List hs = sensH[0];
	    Data.List[] hks = new Data.List[nx];
	    for (int i=0; i<nx; i++)
	    	hks[i] = sensH[i+1];
	    return new SensMatrix(xbase, dx, 
	    	compare.refs(), hs, hks,
		compare.pointWgts(), compare.curveWgts());
	}

	//// local multiple run support
	
	// do multiple runs (parallel if processors available)
	private int multRuns(double[][] x, double[] errs,
	Data.List[] sensH, boolean updateResults) throws Xcept {

	    // prepare job for each run
	    int nruns = errs.length;
	    for (int i=0; i<nruns; i++) 
	    	errs[i] = Double.NaN;
	    MPDispatch.Job jobs[] = new MPDispatch.Job[nruns];
	    for (int i=0; i<nruns; i++) {
	    	int arrInx = i;
		jobs[i] = new SingleRun("opt_" + arrInx, arrInx, x[i], 
		    errs, sensH, updateResults);
	    }

	    // short-circuit 1st run if same as "best"
	    if (Util.isSame(x[0], results.bestX)) {
	    	errs[0] = singleRunUpdate(storeInxBest(), x[0],
		    0, sensH, updateResults);
		MPDispatch.Job[] jobz = new MPDispatch.Job[nruns-1];
		for (int i=1; i<nruns; i++)
		    jobz[i-1] = jobs[i];
		jobs = jobz;
	    }

	    // send jobs to dispatcher
	    MPDispatch dispatch = new MPDispatch("optim", jobs, this);
	    dispatch.run(nproc);

	    // return number completed
	    int ct = nruns;
	    if (results.bestErr < args.errTol) 
	    	while (ct>0 && Double.isNaN(errs[ct-1])) 
		    ct--;
	    return ct; 
	}
	    
	// MPDispatch.Monitor method(s)
	public synchronized void jobStarted(MPDispatch.Job job) {
//	    System.err.println("job started: " + job.jobName());
	}
	public synchronized void jobCompleted(MPDispatch.Job job) {
//	    System.err.println("job done: " + job.jobName());
	}

	//// local single run support
	
	// run model once,  update
	private double singleRun(int threadInx, double[] x,
	int sensInx, Data.List[] sensH, boolean updateResults) throws Xcept {
	    NamedVal.NList nvals = new NamedVal.NList();
	    for (int i=0; i<nx; i++) {
	    	NamedVal nval = NamedVal.create(args.xname[i], x[i]);
		nvals.add(nval);
	    }
	    int storeInx = storeInx(threadInx);
	    int np = 1; // # processors for single run

	    try {
	    	lsmodel.rtmodel().run(threadInx, np, storeInx, nvals);
	    } catch (Exception e) {
		results.termMsg = "Model evaluation " 
		    + results.args.xname[0] + "=" + Util.pretty(x[0])
		    + ": " + e.getMessage();
		throw Xcept.wrap(e);
	    }

	    return singleRunUpdate(storeInx, x, 
	    	sensInx, sensH, updateResults);
	}

	// update results, return rmsError
	//   synchronized for safety during mult runs
	//   s/b fast compared to model run, so synch is OK
	private synchronized double singleRunUpdate(int storeInx, 
	double[] x, int sensInx, Data.List[] sensH, boolean updateResults) throws Xcept {

	    // create RMS data comparison 1st time
	    if (compare == null) createCompare(storeInx);

	    // calculate sens data, if needed
	    if (sensH != null)
	        sensH[sensInx] = getSensH(storeInx);

	    // if calcCovMat, skip results update
	    if (!updateResults) return Double.NaN;

	    // # runs 
	    lsmodel.jobStat.nrunsDone++;

	    // get data from model
	    Data.List dlist = new Data.List(nmatches());
	    for (int i=0; i<nmatches(); i++) {
		Data d = lsmodel.getData(storeInx, matchExprs[i]);
		dlist.add(d);
	    }
	    compare.setData(dlist);

	    // calc residual error
	    double err = compare.rmsError();

	    // update model stores, results
	    //    unless this is short-circuited multirun
	    if (storeInx != storeInxBest() && results.newBest(err)) {
	        lsmodel.rtmodel().copyStore(storeInx, storeInxBest());
		lsmodel.jobStat.nrunsBest = lsmodel.jobStat.nrunsDone;
		results.bestCompare = compare;
	    }

	    // update results
	    results.addResult(err, x); 

	    // return error
	    return err;
	}

	// create DataCompare
	private void createCompare(int storeInx) throws Xcept {
	    Data.List refList = new Data.List(jobInfo.refData);
	    compare = new DataCompare(refList);
	    compare.setCurveWgts(jobInfo.curveWgts);

	    // point weights
	    Data.List pwgts = new Data.List(nmatches());
	    for (int i=0; i<nmatches(); i++) {
		ASQuery q = lsmodel.parseQuery(jobInfo.pointWgts[i]);
		Data pwgt = lsmodel.getData(storeInx, q);
		pwgts.add(pwgt);
	    }
	    compare.setPointWgts(pwgts);
	}
	
	// get sensitivity matrix data list from data store
	private Data.List getSensH(int storeInx) throws Xcept {
	    int nd = compare.n();
	    Data.List dlist = new Data.List(nd);
	    for (int i=0; i<nd; i++) {
	    	Data d = lsmodel.getData(storeInx, matchExprs[i]);
		dlist.add(d);
	    }
	    return compare.matchRefs(dlist);
	}

	//// simple query
	private int nmatches() { return jobInfo.matchExprs.length; }
	public OptimResults results() { return results; }
	public int storeInx(int threadInx) { return threadInx; }
	public int storeInxBest() { return threadInxBase; }
	public int threadInxWorkBase() { return threadInxBase + 1; }
	public int maxProc() {
	    return opt.allowMP() ? lsmodel.maxProc() : 1;
	}

	//// LSModelOptim.SingleRun
	public class SingleRun implements MPDispatch.Job {
	    private String jobName;
	    private int arrInx;
	    private double[] x;
	    private double[] errs;
	    private Data.List[] sensH;
	    private boolean updateResults;
	
	    // constructor
	    public SingleRun(String j, int inx, double[] xx, 	
	    double[] errs, Data.List[] sensH, boolean updateResults) {
		jobName = j;
		arrInx = inx;
		x = xx;
		this.errs = errs;
		this.sensH = sensH;
	 	this.updateResults = updateResults;
	    }
	
	    // name
	    public String jobName() { return jobName; }

	    // run
	    public void jobRunX(int workerInx) throws Xcept {
		// skip run if err tol met and not in sens mat calc
		boolean skip = results.bestErr < args.errTol;
		if (sensH != null) skip = false;
		if (skip) return;
		    
		int threadInx = threadInxWorkBase() + workerInx;
	        double err = singleRun(threadInx, x, 
		    arrInx, sensH, updateResults);
		errs[arrInx] = err;
	    }
		
	    // skip
	    public void jobSkip() { /* ??? */ }

	    // cancel
	    public void jobCancel() { /* ??? */ }
	}
}

