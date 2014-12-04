/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// top level control of batch application

package JSim.text;

import java.io.*;	// File and PrintStream
import java.net.URL;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.project.*;

public class BAppl extends PApplication {
	// project config
	public String     modelStr; // model to process (otherwise 1st)
	public StringList projImports; // project:tabnames to import
	public StringList imageStr; // model image files to load
	public StringList funcStr;  // funcgen names to create
	public StringList plotStr; // plotpage names to create
	public StringList nestedStr; // nested plots names to create
	public StringList rmStr; // project items to remove
	public String loadModel;  // model code to load
	public String loadParSet; // parset name to load
	public String loadXSParFile; // xsim .par file to load
	public String modelRTML; // RTML file to import
	public StringList inputStr; // input var assignments
	public StringList filterDataSets; // dataset names to filter
	public String storeParSet; // parset name to store
	public String storePDataSet; // store output in dataset

	// run control
	public boolean loops;	// true if loops on
	public boolean sens;	// true if sens instead of run
	public boolean optim;	// true if optimize before run
	public boolean monte;   // run monte-carlo analysis instead
	public boolean fwdIC;	// if true, forward ICs after run
	public NamedVal.List fwdICDoms; // domain vals to forward
	public int nruns;   // # runs for each model (default=1)

	// output control
	public String outFile;	// output instead of stdout
	public boolean query;
	public StringList queryStr;
	public StringList outputStr;
	public String formatStr; // format name
	public boolean outputBlocked; // selected blocked output
	public int outputBlockMin; // lmin for blocked output
	public int outputBlockMax; // lmax for blocked output
	public int outputDim; // select output dimension
	public boolean outputFinal; // output only final values
	public boolean outputReport; // output optim/monte report
	public boolean outputReports; // output all monte optim reports?
	public boolean outputPlots; // take output from plots?
	public boolean outputNested; // output nested data
	public String outputDataset; // take output from dataset
	public boolean outputFgenPreview; // fgen preview output
	public boolean outputRTML; // RTML output
	public int outputText;	// text to dump, or -1 if none
	public String outputTextVariant; // PlanWriter plugin, if any
	public boolean outputProj; // output project
	public boolean outputModelParset; // output last parset
	public String outputParset; // output specified parset
	public boolean outputProfile; // Profile output
	public StringList projExports; // tabnames to export
	public boolean outputCrash; // do output even if run crashes
	public int precision; // DataWriter numeric precision 
	public double zeroThresh; // DataWriter zero threshold
	public String outputEncoding; // DataWriter encoding
	public boolean timing;  // dump timing

	// test flags
	public String queryTestExpr;
	public long queryTestPause;
	
	// args constructor
	public BAppl(String[] args) throws Xcept {
	    super(args);
	}

	// parse extra args
	protected void parse_xargs(String[] args) throws Xcept {
	    Util.verbose("Starting jsbatch version " + 
	    	Util.version() + " on " + Util.javaOSVersion());

	    // default settings
	    projImports = new StringList(4);
	    imageStr = new StringList(4);
	    funcStr = new StringList(args.length);
	    plotStr = new StringList(args.length);
	    nestedStr = new StringList(args.length);
	    rmStr = new StringList(4);
	    inputStr = new StringList(args.length);
	    filterDataSets = new StringList(4);
	    projExports = new StringList(4);

	    fwdICDoms = new NamedVal.List();

	    loops = false;
	    optim = false;
	    monte = false;
	    nruns = 1;
	    outFile = null;
	    query = false;
	    queryStr = new StringList(args.length);
	    outputStr = null;
	    outputDim = -1;
	    formatStr = "line";
	    outputBlocked = false;
	    outputReport = false;
	    outputReports = false;
	    outputPlots = false;
	    outputDataset = null;
	    outputFgenPreview = false;
	    outputRTML = false;
	    outputText = -1;
	    outputTextVariant = null;
	    outputProj = false;
	    outputCrash = false;
	    precision = Util.SINGLE_PRECISION;
	    zeroThresh = 0;
	    outputEncoding = null;

	    // loop over args
	    if (args == null) args = new String[0];
	    for (int i=0; i<args.length; i++) {
		if (args[i].equals("-model") && i<args.length-1) {
		    modelStr = args[++i];
		} else if (args[i].equals("-iproj")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			projImports.add(args[++i]);
		} else if (args[i].equals("-img")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			imageStr.add(args[++i]);
		} else if (args[i].equals("-func")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			funcStr.add(args[++i]);
		} else if (args[i].equals("-plot")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			plotStr.add(args[++i]);
		} else if (args[i].equals("-nested")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			nestedStr.add(args[++i]);
		} else if (args[i].equals("-rm")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			rmStr.add(args[++i]);
		} else if (args[i].equals("-lmod") && i<args.length-1) {
		    loadModel = args[++i];
		} else if (args[i].equals("-lpar") && i<args.length-1) {
		    loadParSet = args[++i];
		} else if (args[i].equals("-lxpar") && i<args.length-1) {
		    loadXSParFile = args[++i];
		} else if (args[i].equals("-rtml") && i<args.length-1) {
		    modelRTML = args[++i];
		} else if (args[i].equals("-i")) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			inputStr.add(args[++i].replaceAll("@", " "));
		} else if (args[i].equals("-filter") && i<args.length-1) {
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			filterDataSets.add(args[++i]);
		} else if (args[i].equals("-spar") && i<args.length-1) {
		    storeParSet = args[++i];
		} else if (args[i].equals("-sdata") && i<args.length-1) {
		    storePDataSet = args[++i];

		} else if (args[i].equals("-optim")) {
		    optim = true;
		} else if (args[i].equals("-monte")) {
		    monte = true;
		} else if (args[i].equals("-loops")) {
		    loops = true;
		} else if (args[i].equals("-sens")) {
		    sens = true;
		} else if (args[i].equals("-fwdIC")) {
		    fwdIC = true;
		    while (i<args.length-1 && args[i+1].charAt(0) != '-') 
		    	addFwdIC(args[++i]);
		} else if (args[i].equals("-nruns") && i<args.length-1) {
		    nruns = Util.toInt(args[++i]);
		} else if (args[i].equals("-out") && i<args.length-1) {
		    outFile = args[++i];
		} else if (args[i].equals("-odim") && i<args.length-1) {
		    outputDim = Util.toInt(args[++i]);
		} else if (args[i].equals("-ofmt") && i<args.length-1) {
		    formatStr = args[++i];
		} else if (args[i].equals("-oblocked") && i<args.length-2) {
		    outputBlocked = true;
		    outputBlockMin = Util.toInt(args[++i]);
		    outputBlockMax = Util.toInt(args[++i]);
		} else if (args[i].equals("-q")) {
		    query = true;
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			queryStr.add(args[++i]);
		} else if (args[i].equals("-o")) {
	    	    if (outputStr == null)
			outputStr = new StringList(args.length);
		    while (i<args.length-1 && args[i+1].charAt(0) != '-') 
			outputStr.add(args[++i].replaceAll("@", " "));
		} else if (args[i].equals("-oreport")) {
		    outputReport = true;
		} else if (args[i].equals("-oreports")) {
		    outputReport = true;
		    outputReports = true;
		} else if (args[i].equals("-oplot")) {
		    outputPlots = true;
		} else if (args[i].equals("-onested")) {
		    outputNested = true;
		} else if (args[i].equals("-ofgenprev")) {
		    outputFgenPreview = true;
		} else if (args[i].equals("-odataset") && i<args.length-1) {
		    outputDataset = args[++i];
		} else if (args[i].equals("-ortml")) {
		    outputRTML = true;
		} else if (args[i].equals("-oplan")) {
		    outputText = ASModel.TEXT_PLAN;
		    if (i+1<args.length && args[i+1].charAt(0) != '-')
		    	outputTextVariant = args[++i];
		} else if (args[i].equals("-omathml")) {
		    outputText = ASModel.TEXT_MATHML;
		} else if (args[i].equals("-oxmml")) {
		    outputText = ASModel.TEXT_XMML;
		    if (i+1<args.length && args[i+1].charAt(0) != '-')
		    	outputTextVariant = args[++i];		    
		} else if (args[i].equals("-osbml")) {
		    outputText = ASModel.TEXT_SBML;
		    if (i+1<args.length && args[i+1].charAt(0) != '-')
		    	outputTextVariant = args[++i];		    
		} else if (args[i].equals("-ographml")) {
		    outputText = ASModel.TEXT_GRAPHML;
		} else if (args[i].equals("-omml")) {
		    outputText = ASModel.TEXT_MML;
		} else if (args[i].equals("-oantimony")) {
		    outputText = ASModel.TEXT_ANTIMONY;
		} else if (args[i].equals("-ocellml")) {
		    outputText = ASModel.TEXT_CELLML;
		} else if (args[i].equals("-omatlab")) {
		    outputText = ASModel.TEXT_MATLAB;
		} else if (args[i].equals("-oflat")) {
		    outputText = ASModel.TEXT_FLATMML;
		} else if (args[i].equals("-ojava")) {
		    outputText = ASModel.TEXT_JAVA;
		} else if (args[i].equals("-oproj")) {
		    outputProj = true;
		    while (i<args.length-1 && args[i+1].charAt(0) != '-')
			projExports.add(args[++i]);
		} else if (args[i].equals("-opar")) {
		    if (i+1<args.length && args[i+1].charAt(0) != '-')
		    	outputParset = args[++i];
		    else
		        outputModelParset = true;
		} else if (args[i].equals("-oprofile")) {
		    outputProfile = true;
		} else if (args[i].equals("-ocrash")) {
		    outputCrash = true;
		} else if (args[i].equals("-osingle")) {
		    precision = Util.SINGLE_PRECISION;
		} else if (args[i].equals("-odouble")) {
		    precision = Util.DOUBLE_PRECISION;
		} else if (args[i].equals("-oprec") && i<args.length-1) {
		    precision = Util.toInt(args[++i]);
		} else if (args[i].equals("-ozero") && i<args.length-1) {
		    zeroThresh = Util.toDouble(args[++i]);
		} else if (args[i].equals("-oencoding") && i<args.length-1) {
		    outputEncoding = args[++i];
		} else if (args[i].equals("-queryTest") && i<args.length-2) {
		    queryTestExpr = args[++i];
		    queryTestPause = Util.toInt(args[++i]);
		} else if (args[i].equals("-ofinal")) {
		    outputFinal = true;
	    	    if (outputStr == null)
			outputStr = new StringList(args.length);
		    while (i<args.length-1 && args[i+1].charAt(0) != '-') 
			outputStr.add(args[++i]);
		} else if (args[i].equals("-timing")) {
		    timing = true;

		} else throw new Xcept(
		    "jsbatch: illegal switch usage \"" + args[i] + 
		    "\"\n  run with -usage switch to see valid usage");
	    }

	    // output sanity check
	    int ct = 0;
	    if (query) ct++;
	    if (outputProj) ct++;
	    if (outputPlots) ct++;
	    if (outputNested) ct++;
	    if (outputStr != null) ct++;
	    if (outputReport) ct++;
	    if (outputText>=0) ct++;
	    if (outputDataset != null) ct++;
	    if (outputRTML) ct++;
	    if (outputFgenPreview) ct++;
	    if (outputModelParset) ct++;
	    if (outputParset != null) ct++;
	    if (outputProfile) ct++;
	    if (ct>1) throw new Xcept(
		"Multiple conflicting output requests (-q, -o*)");

	    // oreport sanity check
	    if (outputReport && !optim && !monte) throw new Xcept(
	    	"-oreport requires either -optim or -monte");

	    // run sanity check
	    ct = 0;
	    if (loops) ct++;
	    if (sens) ct++;
	    if (optim) ct++;
	    if (monte) ct++;
	    if (ct>1) throw new Xcept(
	    	"Multiple run types requested (-loops, -sens, -optim, -monte)");
	    
	    // local args update for sandbox
	    if (outFile != null && sandbox != null)
		sandbox.writePath.add(outFile);
	}

	// switch usage message
	public String usage() throws Xcept {
	    String res = "jsbatchUsage.txt";
	    URL url = getClass().getResource(res);
	    if (url == null) throw new Xcept(
		"Resource \"" + res + "\" not found");
	    return UtilIO.readText(url);
	}	    

	// add domain=value to fwcICDoms
	private void addFwdIC(String s) throws Xcept {
	    int qx = s.indexOf("=");
	    if (qx<0) throw new Xcept(
	        "-fwdIC domain=value expected");
	    String n = s.substring(0,qx);
	    String v = s.substring(qx+1);
	    NamedVal nval = NamedVal.create(n, Util.toDouble(v));
	    fwdICDoms.add(nval);
	}
}

