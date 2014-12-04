/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Local Server incarnation of run-time model

package JSim.lserver;

import java.io.*;
import java.lang.reflect.*;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import JSim.jruntime.*;
import JSim.aserver.*;
import JSim.mathml.*;
import JSim.data.*;
import JSim.nml.opt.*;

public class LSModel  implements DiagInfo, ASModel, 
 MPDispatch.Monitor, ASServer.Messenger, Model.Translator {

	// static state
	private static int seq = 100; // build sequence #

	// preserved across builds
	private LSServer server; // for this server

	// built state
	private ASInfo.Build buildInfo; // build info
	private boolean cancelBuild; // cancel flag
	private String name;	// model name
	private Model flatModel;	// flattened MML model
	private LSPlan plan;	// planning for model above
	private RTModel rtmodel;	// run-time model
	private String mmlText; // .mml text, if any
	private File flatFile;	// .flat text, if any
	private File javaFile;  // java run-time code
	private String className; // model class name
	private int maxBuildTime; // in seconds
	private long buildStartTime; // in milliseconds
	private String[] modelTextCache; // cached getText queries
	private String[][] modelTextWarningCache; // cached getText warnings

	// SBML-special planning (no __init vars)
	private LSPlan sbmlPlan;

	// run-time state
	private int storeMode; // see ASModel constants
	private ASInfo.JobInfo jobInfo; // running job info
	protected ASInfo.Status jobStat; // running job status
	private LSModelOptim optim; // latest optimization
	private LSModelMopt mopt; // latest mult-optim data
	private MPDispatch dispatch; // job dispatcher

	// constructor builds and instantiates model
	public LSModel(LSServer s) {
	    server = s;
	    unbuildRT();
	}
	
	// unbuild rtmodel
	public void unbuildRT() {
	    flatModel = null;
	    plan = null;
	    rtmodel = null;
	    mmlText = null;
	    flatFile = javaFile = null;
	    storeMode = SINGLE;
	    optim = null;
	    modelTextCache = null;
	    sbmlPlan = null;
	    jobStat = new ASInfo.Status(0);
	    modelTextCache = new String[ASModel.TEXT_NAMES.length];
	    modelTextWarningCache = new String[ASModel.TEXT_NAMES.length][];
	    dispatch = null;
	}    	    

	// (re)build rtmodel (scrubs on exit, even if exception)
	public void buildRT(ASInfo.Build binfo) throws Xcept {
	    unbuildRT();
	    cancelBuild = false;
	    buildInfo = binfo;
	    jobInfo = binfo;
	    maxBuildTime = binfo.options.intVal("maxBuildTime", 120);
	    int smax = server.maxBuildTime();
	    if (smax > 0 && smax < maxBuildTime)
	    	maxBuildTime = smax;
	    buildStartTime = System.currentTimeMillis();
	    Util.verbose("==== Build directory is " + server.buildDir);
	    name = buildInfo.name.replace('.', '_');
	    className = "JS" + (seq++) + name;
	    try {
		buildRT1();
	        server.scrubBuildDir();
	    } catch (Exception e) {
	        server.scrubBuildDir();
		throw Xcept.wrap(e);
	    }
	}
	
	// exception protected build
	private void buildRT1() throws Xcept {
	    if (buildInfo.sourceType == ASModel.TEXT_MML)
	    	buildPlan();
	    buildJava();
	    buildClass();
	}
	
	// parse model, flatten, create Plan
	private void buildPlan() throws Xcept {
	    flatFile = new File(server.buildDir,
		className + ".flat");
	    plan = new LSPlan(server, buildInfo.modelSource,
	        flatFile, buildInfo.options);
	    plan.makePlan();
	    flatModel = plan.getFlatModel();
	}

	// create Java code for model
	private void buildJava() throws Xcept {
	    javaFile = new File(server.buildDir, className + ".java");

	    // if source is MML, compile natives, write Java from plan
	    if (buildInfo.sourceType == ASModel.TEXT_MML) {
		// compile native functions, one at a time
		XFunc.NList funcs = flatModel.neededFuncs();
		for (int i=0; i<funcs.size(); i++) {
		    XFunc f = funcs.xfunc(i);
		    if (! (f instanceof SourceFunc)) continue;
		    switch (f.lang()) {
		    case XFunc.C: 
		    case XFunc.CC: 
		    case XFunc.FORTRAN:
			server.nativeCompiler.compile(
			    className, (SourceFunc) f);
		    }
		}

		// write Java from plan
		Util.verbose("\tWriting generated Java model code to " 
		    + javaFile);
	        plan.writeJava(className, javaFile);

	    // if course is Java, write replaces text
	    } else if (buildInfo.sourceType == ASModel.TEXT_JAVA) {
		Util.verbose("\tWriting stored Java model code to " 
		    + javaFile);
		String modelSource = buildInfo.modelSource.replaceAll(
		    ASModel.JSIM_MODEL_CLASS, className);
		UtilIO.writeText(javaFile, modelSource);
	    }
	}
	
	// compile Java and instantiate
	private void buildClass() throws Xcept {
	    // compile Java run-time class
	    Util.verbose("==== Compiling Java model " + javaFile);
	    server.javac().compile(javaFile, System.err);

	    // compile native code, if any, check sandbox
	    if (buildInfo.sourceType == ASModel.TEXT_MML) {
	    	if (server.sandbox != null)
		    flatModel.checkSandbox();

		XFunc.NList funcs = flatModel.neededFuncs();
		for (int i=0; i<funcs.size(); i++) {
		    XFunc f = funcs.xfunc(i);
		    if (! (f instanceof SourceFunc)) continue;
		    switch (f.lang()) {
		    case XFunc.C: 
		    case XFunc.CC: 
		    case XFunc.FORTRAN:
			server.nativeCompiler.compile(
			    className, (SourceFunc) f);
		    }
		}
	    }

	    // create model instance
  	    Util.verbose("==== Instantiating Java model " + className);
            try {
          	Class<?> model_class = server.classLoader.loadClass(className);
           	if (! RTModel.class.isAssignableFrom(model_class)) throw new Exception
		    ("Class " + className + " does not extend RTModel");
	        Class[] argc = new Class[] 
		    { UnitNList.class, ASServer.Messenger.class };
 		Constructor cons = model_class.getConstructor(argc);
		UnitNList sunits = null; // v2.01+: don't merge sysUnits 
           	Object[] args = new Object[] { sunits, this };
           	rtmodel = (RTModel) cons.newInstance(args);
		rtmodel.setMaxProc(maxProc());
		rtmodel.setOptimFactory(server.optimFactory());
            } catch (InvocationTargetException e) {
		e.getTargetException().printStackTrace();
            	throw new Xcept(e, "Invocation target error instantiating " 
		    + className);
            } catch (NoSuchMethodException e) {
            	throw new Xcept(e, "Class " + className + 
		    " must be public and have public CONSTRUCTOR with one argument");
            } catch (IllegalAccessException e) {
            	throw new Xcept(e, "Class " + className + 
		    " must be public and have PUBLIC constructor with one argument");
            } catch (InstantiationException e) {
            	throw new Xcept(e,
		    "Error instantiating " + className);
            } catch (Throwable e) {
            	throw new Xcept(e,
		    "Internal error instantiating " + className
		    + ": " + e);
            }
	}

	// get build errors & warnings
	public String[] getBuildAlerts() { 
	    if (plan == null) return null;
	    return plan.getBuildAlerts();
	}

	// BuildControl cancel method (obsolete???)
	public String buildCancelMessage() {
	    if (cancelBuild) return "user";
	    long delta = 
		System.currentTimeMillis() - buildStartTime;
	    if (maxBuildTime > 0 && delta > maxBuildTime*1000) 
		return "timeout (" + maxBuildTime + " secs)";
	    return null;
	}

	// check is built?
	private void checkBuilt(String s) throws Xcept {
	    if (isBuilt()) return;
	    throw new Xcept(this, s + 
	    	"() called while model not compiled");
	}

	// query
	public String diagInfo() { return "LSModel " + name; }
	public final boolean isBuilt() { return rtmodel != null; }
	public ASVar.List getASVars() throws Xcept { 
	    checkBuilt("getASVars");
	    return rtmodel.getVars(); 
	}
	public ASVar getASVar(String n) throws Xcept {
	    checkBuilt("getASVar");
	    return rtmodel.getVar(n);
	}

	public ASQuery parseQuery(String s) throws Xcept {
	    checkBuilt("parseQuery");
	    try {
		return rtmodel.getVar(s);
	    } catch (Xcept e) {
	    	return new RTQuery(rtmodel, s);
	    }
	}
	public Data getData(int i, ASQuery query) throws Xcept {
	    checkBuilt("getData");
	    if (query instanceof RTVar)
	    	return rtmodel.getData(i, (RTVar) query);	    
	    else if (query instanceof RTQuery) 
	    	return rtmodel.getData(i, ((RTQuery) query).expr());
	    else 
	    	throw new Xcept("LSModel.getData() requires RTQuery");
	}
	public MoptData getMoptData() {
	    if (mopt == null) return null;
	    return mopt.moptData();
	}
 	public UnitNList units() throws Xcept{ 
	    checkBuilt("units");
	    return rtmodel.units; 
	}

	// get various types of text
	public String getText(int type, String variant) 
	throws Xcept {
	    try {
		boolean useCache = useCache(type, variant);
		if (useCache && modelTextCache[type] != null)
		    return modelTextCache[type];
	    	String text = getNewText(type, variant);
		if (useCache) {
		    modelTextCache[type] = text;
		    if (usesTranslator(type)) 
			modelTextWarningCache[type] = server.getTranslatorWarnings();
		}
		return text;
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}
	
	// get text warnings
	public String[] getTextWarnings(int type, String variant) {
	    boolean useCache = useCache(type, variant);
	    return useCache ? modelTextWarningCache[type] : null;
	}

	// use text/warning caches?
	private boolean useCache(int type, String variant)  {
	    if (modelTextCache == null) return false;
	    if (type < 0) return false;
	    if (type >= modelTextCache.length) return false;
	    if (! Util.isBlank(variant)) return false;
	    return true;
	}

	// does getText use LSTranslator?
	private boolean usesTranslator(int type) {
	    switch (type) {
	    case ASModel.TEXT_SBML: return true;
	    case ASModel.TEXT_CELLML: return true;
	    case ASModel.TEXT_ANTIMONY: return true; // ???
	    }
	    return false;
	}

	// get text, unknown Exception type
	private String getNewText(int type, String variant) 
	throws Exception {
	    switch(type) {
	    case ASModel.TEXT_PLAN:
		if (plan == null) throw new Xcept(
		    "Plan text not available");
		return plan.getPlanText(variant);
	    case ASModel.TEXT_MATHML:
		if (flatModel == null) throw new Xcept(
		    "Flat model text not available");
	    	Document doc = UtilXML.createDoc("html");
	    	Element html = doc.getDocumentElement();
		html.setAttribute("xmlns",
		    "http://www.w3.org/1999/xhtml");
		Element body = doc.createElement("body");
		html.appendChild(body);
	    	MLWriter mwrt = new MLWriter(body);
		mwrt.setParagraphs(true);
	    	mwrt.add(flatModel);
	    	XMLWriter xwrt = new XMLWriter();
		StringWriter swrt = new StringWriter();
	    	xwrt.write(doc, swrt);
		return swrt.toString();
	    case ASModel.TEXT_XMML:
	    	LSPlan xplan = plan;
		if (variant != null && variant.equals("sbml"))
		    xplan = sbmlPlan();
		if (xplan == null) throw new Xcept(
		    "Plan XMML text not available");
		return xplan.getXMMLText();
	    case ASModel.TEXT_GRAPHML:
		if (plan == null) throw new Xcept(
		    "Plan GraphML text not available");
		return plan.getGraphMLText();
	    case ASModel.TEXT_MML: 
		if (mmlText == null) throw new Xcept(
		    "MML text not available");
		return mmlText;
	    case ASModel.TEXT_FLATMML: 
		if (flatFile == null || ! flatFile.exists()) 
		    throw new Xcept("Flat MML text not available");
		return UtilIO.readText(flatFile); 
	    case ASModel.TEXT_JAVA: 
		if (javaFile == null || ! javaFile.exists()) 
		    throw new Xcept("Java text not available");
		return UtilIO.readText(javaFile);
	    case ASModel.TEXT_SBML:
	    	String xmml = getText(ASModel.TEXT_XMML, "sbml");
		return server.translateModelText(
		    ASModel.TEXT_XMML, ASModel.TEXT_SBML, xmml, variant);
	    case ASModel.TEXT_ANTIMONY:
	    	String sbml = getText(ASModel.TEXT_SBML, null);
		return server.translateModelText(
		    ASModel.TEXT_SBML, ASModel.TEXT_ANTIMONY, sbml, variant);
	    case ASModel.TEXT_CELLML:
	    	xmml = getText(ASModel.TEXT_XMML, "sbml");
		return server.translateModelText(
		    ASModel.TEXT_XMML, ASModel.TEXT_CELLML, xmml, variant);
	    case ASModel.TEXT_MATLAB:
	    	sbml = getText(ASModel.TEXT_SBML, null);
		return server.translateModelText(
		    ASModel.TEXT_SBML, ASModel.TEXT_MATLAB, sbml, variant);
	    default: 
	    	throw new Xcept("Unknown getText ID=" + type);
	    }	
	}
	
	// get SBML-variant plan (version 2 only)
	private LSPlan sbmlPlan() throws Xcept {
	    if (sbmlPlan != null) return sbmlPlan;
	    if (buildInfo == null) return null;
	    if (buildInfo.sourceType != ASModel.TEXT_MML)
	    	return null;
	    File flatFile = new File(server.buildDir,
	    	className + "_SBML.flat");
	    NamedVal.NList options = new NamedVal.NList(
	        buildInfo.options.info());
	    options.setVal("makeDEICParms", false);
	    sbmlPlan = new LSPlan(server, buildInfo.modelSource,
	    	flatFile, options);
	    sbmlPlan.makePlan();
	    return sbmlPlan;
	}
	    
	// max proc to use for model runs
	protected int maxProc() {
	    int n = (rtmodel != null && ! rtmodel.allowMPRuns()) ?
	    	1 : server.maxProc();
	    return n;
	}

	// single run of model
	public void singleRun(NamedVal.NList runVals) throws Xcept {
	    checkCompiled();
	    rtmodel.cancelRun = false;
	    storeMode = ASModel.SINGLE;
	    jobInfo = null;

	    // initialize job
	    jobStat = new ASInfo.Status(1);
	    jobStat.mode = ASModel.SINGLE;
	    jobStat.nruns = 1;
	    server.prepNativeMethods();
	    
	    // one run 
	    rtmodel.initMemory(runVals);
	    rtmodel.setMaxProc(maxProc());
	    rtmodel.allocStores(1);
	    rtmodel.setRunVals(runVals);
            rtmodel.run(1, maxProc(), 0, null);

	    // hack job done
	    jobStatDone();
	}

	// run model loops
	public void loopsRun(ASInfo.Loops loops) throws Xcept {
	    checkCompiled();
	    rtmodel.cancelRun = false;
	    storeMode = ASModel.LOOPS;
	    jobInfo = loops;
	    
	    // initialize job
	    jobStat = new ASInfo.Status(loops.n());
	    jobStat.mode = ASModel.LOOPS;
	    jobStat.nruns = loops.n();
	    jobStat.runName = "parallel loops";
	    server.prepNativeMethods();

	    // prepare parallel jobs
	    MPDispatch.Job jobs[] = new MPDispatch.Job[loops.n()];
	    for (int i=0; i<loops.n(); i++) 
		jobs[i] = new SingleRun(
		    loops.runNames[i], i, loops.nvals[i]);

	    // run parallel jobs
	    rtmodel.initMemory(loops.baseVals);
	    rtmodel.setMaxProc(maxProc());
	    rtmodel.allocStores(loops.runNames);
	    rtmodel.setRunVals(loops.baseVals);
	    dispatch = new MPDispatch("loops", jobs, this);
	    dispatch.run(maxProc());

	    // hack job done
	    jobStatDone();
	    jobStat.nrunsDone = jobStat.nruns;
	}
		
	//// MPDispatch.Monitor callbacks
	public synchronized void jobStarted(MPDispatch.Job job) {
	    jobStat.runName = job.jobName();
	}
	public synchronized void jobCompleted(MPDispatch.Job job) {
	    jobStat.nrunsDone++;
	}

	// run sensitivity analysis
	public void sensRun(ASInfo.Sens sens) throws Xcept {
	    checkCompiled();
	    rtmodel.cancelRun = false;
	    storeMode = ASModel.SENS;
	    jobInfo = sens;

	    // initialize job
	    jobStat = new ASInfo.Status(1 + sens.parNames.length);
	    jobStat.mode = ASModel.SENS;
	    jobStat.nruns = 1 + sens.parNames.length;
	    server.prepNativeMethods();

	    // unperturbed run
	    rtmodel.initMemory(sens.baseVals);
	    rtmodel.setMaxProc(maxProc());
	    rtmodel.allocStores(sens.parNames, sens.deltas);
	    rtmodel.setRunVals(sens.baseVals);
	    jobStat.runName = null;
	    rtmodel.run(1, 1, 0, null);
	    jobStat.nrunsDone++;

	    // prepare perturbed values based on 1st run
	    int n = sens.parNames.length;
	    MPDispatch.Job jobs[] = new MPDispatch.Job[n];
	    for (int i=0; i<n; i++) {
	        String pname = sens.parNames[i];
		ASQuery q = parseQuery(pname);
		Data data = getData(0, q);
	        double val = data.realVal(0) + sens.deltas[i];
		if (Double.isNaN(val)) throw new Xcept(
		    "Sensitivity parameter " + pname + "=NaN");
		NamedVal nval = NamedVal.create(pname, val);
		NamedVal.NList nvals = new NamedVal.NList();
		nvals.add(nval);
	        jobs[i] = new SingleRun(
		    "sens " + pname, i+1, nvals);
	    }

	    // parallel perturbed runs
	    dispatch = new MPDispatch("sensitivity",
	    	jobs, this);
  	    dispatch.run(maxProc());
	
	    // hack job done
	    jobStatDone();
	    jobStat.nrunsDone = jobStat.nruns;
	}
		
	// run optimizer
	public void optimRun(ASInfo.Optim jinfo) throws Xcept {
	    checkCompiled();
	    rtmodel.cancelRun = false;
	    storeMode = ASModel.OPTIM;
	    jobInfo = jinfo;

	    // initialize job
	    jobStat = new ASInfo.Status(2);
	    jobStat.mode = ASModel.OPTIM;
	    jobStat.nruns = 1 + jinfo.args.maxCalls;
	    server.prepNativeMethods();

	    // call optimizer
	    optim = null;
	    optim = new LSModelOptim(this, jinfo, 0, 0);
	    rtmodel.initMemory(jinfo.baseVals);
	    int nproc = optim.maxProc();
	    rtmodel.setMaxProc(maxProc());
	    rtmodel.allocStores(1+nproc);
	    rtmodel.setRunVals(jinfo.baseVals);
	    optim.optimize();
	
	    // hack job done
	    jobStatDone();
	}

	// run func image
	public void moptRun(ASInfo.Mopt jinfo) throws Xcept {
	    mopt = null;
	    checkCompiled();
	    rtmodel.cancelRun = false;
	    storeMode = ASModel.MOPT;
	    jobInfo = jinfo;

	    // initialize job
	    jobStat = new ASInfo.Status(2);
	    jobStat.mode = ASModel.MOPT;
	    jobStat.nruns = 1 + jinfo.optim.args.maxCalls;
	    server.prepNativeMethods();

	    // start job
	    optim = null;
	    rtmodel.initMemory(jinfo.optim.baseVals);
	    int nproc = maxProc();
	    rtmodel.setMaxProc(maxProc());
	    rtmodel.allocStores(2*nproc + 1); // 1 single + (1 best + 1 work)/proc
	    rtmodel.setRunVals(jinfo.optim.baseVals);
	    mopt = new LSModelMopt(this, jinfo, nproc);
	    mopt.run();
	
	    // hack job done
	    jobStatDone();
	}

	// check model is compiled
	private void checkCompiled() throws Xcept {
	    if (rtmodel == null) throw new Xcept(this,
		"Can't start job.  Model is not compiled.");
	}
	
	// get job status
	public ASInfo.Status getJobStatus() {
	    if (jobStat == null) return null;
	    if (jobStat != null && rtmodel != null && jobStat.runStats != null) 
	    	for (int i=0; i<jobStat.runStats.length; i++) 
	    	    jobStat.runStats[i] = rtmodel.getRunStat(i);
	    return jobStat;
	}

	// skip next loop
	public void nextLoop() {
	    if (storeMode != ASModel.LOOPS) return;
	    if (dispatch == null) return;
	    dispatch.skipOneJob();
	}

	// cancel build or run
	public void cancelJob() {
	    cancelBuild = true;
	    if (rtmodel != null) rtmodel.cancelRun = true; 
	}

	// allocation query
	public int storeMode() { return storeMode; }
	public ASInfo.JobInfo getJobInfo() { return jobInfo; }
	public int nstores() { 
	    if (! isBuilt()) return 0;
	    return rtmodel.nstores(); 
	}

	// get store name
	public String getStoreName(int i) {
	    if (i<0 || i>=nstores()) return null;
	    return rtmodel.getStoreName(i);
	}

	// optim results
	public OptimResults optimResults() {
	    if (optim != null) return optim.results();
	    return null; 
	}

	// note termination time in jobStat
	public void jobStatDone() {
	    try {
	    	jobStat.stopTime = System.currentTimeMillis();
	    } catch (Exception e) {	
	    }
	}

	// get profile data
	public ProfileData getProfile() {
	    if (! isBuilt()) return null;
	    if (jobStat == null) return null;
	    if (jobStat.mode == ASModel.COMPILE) return null;
	    ProfileData prof = rtmodel.getProfile();
	    prof.startTime = jobStat.startTime;
	    prof.stopTime = jobStat.stopTime;
	    switch (jobStat.mode) {
	    case ASModel.SINGLE: prof.desc = "single run"; break;
	    case ASModel.LOOPS: prof.desc = "loops run"; break;
	    case ASModel.OPTIM: prof.desc = "optimization run"; break;
	    case ASModel.SENS: prof.desc = "sensitivity run"; break;
	    case ASModel.MOPT: prof.desc = "multiple optimization run"; break;
	    default: prof.desc = "unknown"; break;
	    }
	    return prof;
	}

	// package query
	protected LSServer server() { return server; }
	protected RTModel rtmodel() { return rtmodel; }
	protected ASInfo.Build buildInfo() { return buildInfo; }

	// solver/unit flags
	public ASModel.Flags getFlags() {
	    if (rtmodel == null) return new ASModel.Flags(); 
	    return rtmodel.getFlags();
	}

	// set func gens
	public void setFuncGenNames(String[] names) throws Xcept {
	    if (isBuilt())
	    	rtmodel.setFuncGenNames(names);
	}

 	// unique model identifier within ASServer
  	public String modelID() { return "lsmodel" + hashCode(); }

 	// transfer message to server's messenger
	public void message(ASInfo.Message msg) {
	    if (server.messenger == null) return;
	    msg.modelID = modelID();
	    server.messenger.message(msg);
	}

	// Model.Translator: translate antimony 2 MML
	public String antimony2MML(String text) throws Xcept {
	    return server.translateModelText(ASModel.TEXT_ANTIMONY,
	        ASModel.TEXT_MML, text, null);
	}

	//// LSModel.SingleRun
	public class SingleRun implements MPDispatch.Job {
	    private String jobName;
	    private int storeInx;
	    private NamedVal.NList nvals;
	    private boolean skipRun;
	
	    // constructor
	    public SingleRun(String j, int i, NamedVal.NList n) {
		jobName = j;
	    	storeInx = i;
	    	nvals = n;
	    }
	
	    // name
	    public String jobName() { return jobName; }

	    // run
	    public void jobRunX(int workerInx) throws Xcept {
	    	if (!skipRun)
		    rtmodel.run(workerInx+1, 1, storeInx, nvals);
	    }
	
	    // skip
	    public void jobSkip() {
	    	skipRun = true;
		rtmodel.setSkipRun(storeInx);
	    }
	
	    // cancel
	    public void jobCancel() { /* ??? */ }
	}
}
