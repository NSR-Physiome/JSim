/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim functional image generator

package JSim.text;

import java.io.*;	// File and PrintStream
import java.util.StringTokenizer;
import java.net.URL;
import java.awt.event.*;
import javax.swing.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.project.*;

class FimMain implements ActionListener {

	// command args
	public PApplication appl;
	public JSReadable projFile; // project file
	public JSReadable matchFile;
	public String modelName; // 
	public StringList fgenVars; // fgen variables
	public StringList fgenCurves; // fgen curve names
	public JSReadable.List fgenFiles; // files for input assigns
	public StringList inputs; // input assigns
	public File outFimFile;  // output FIM file
	public File outFitFile; // output fits file
	public File logFile; // opt results,  if selected
	public String fmtName = "column"; // FIM output format
	public String fitFmtName = "JSML"; // FIT output format
	public int outPrec = Util.SINGLE_PRECISION;
	public StringList prefixes; // ref data prefixes
	public StringList excludes; // ref data exclusions
	public boolean outProj;  // output project file?
	public boolean noabort;  // continue if some optims fail
	public boolean conflim;  // calculate confidence limits
	public boolean silent = false; // run silent, run deep
	public boolean usage = false; // print usage message, quit
	public int maxProc = 1;
	public int maxProcDebug = 0;
	public int cancelDelay = -1;
	public String serverName = null;
	public String buildDirSfx = null;
	public boolean outRMS; // output RMS error in FIM output?

	// runtime fields
	private PModelFim pfim;
	private PModelMoptJob pjob;
	private boolean[] segProc; // segment processed flags
	private MoptData moptData;
	private PrintWriter logwrt;

	// constructor
	public FimMain(String[] args) throws Exception {
	    parseArgs(args);
	    if (usage) {
	    	usage();
		return;
	    }

	    // create PApplication 
	    if (!silent) Util.verbose = true;
	    StringList pargs = new StringList();
	    if (serverName != null) {
	    	pargs.add("-server");
		pargs.add(serverName);
	    } 
	    if (buildDirSfx != null) {
	    	pargs.add("-builddirsfx");
		pargs.add(buildDirSfx);
	    }
	    appl = new PApplication(pargs);
	    NamedVal nval = (maxProcDebug > 0) ?
	    	NamedVal.create("maxProcDebug", maxProcDebug) :
		NamedVal.create("maxProc", maxProc);
	    appl.server().setProperty(nval);

	    // create data output formats (check for errors)
	    DataFormat fimFmt = appl.dataFormats().format(fmtName);
	    DataFormat fitFmt = appl.dataFormats().format(fitFmtName);

	    // create project, fim generator
	    Project proj = new Project("proj", appl);
	    proj.importXML(projFile);
	    pfim = new PModelFim(proj);
	    if (modelName == null)
	    	pfim.selectModel();
	    else
	        pfim.selectModel(modelName);

	    // set match data
	    PDataSet matchDataSet = (PDataSet) proj.load(matchFile);
	    int nmatches = prefixes.size();
	    if (nmatches == 0) 
	    	pfim.setMatchData(0, matchDataSet, null, excludes);
	    else
	        for (int m=0; m<nmatches; m++) 
		    pfim.setMatchData(m, matchDataSet, 
		    	prefixes.str(m), excludes);

	    // set inputs
	    for (int i=0; i<inputs.size(); i++) {
	    	String input = inputs.str(i);
		int inx = input.indexOf('=');
		String vname = input.substring(0, inx);
		String expr = input.substring(inx+1);
		Control vc = pfim.pmodel().vars().control(vname);
		if (vc == null) throw new Xcept(
		    "Input variable <" + vname + "> not found");
		vc.setVal(expr);
	    }

	    // set func gens
	    int nfgens = fgenVars.size();
	    for (int i=0; i<nfgens; i++) {
	    	JSReadable r = fgenFiles.readable(i);
		PDataSet dataSet = (r == null) ? 
		    matchDataSet : (PDataSet) proj.load(r);
		pfim.setFuncGen(fgenVars.str(i),
		    dataSet, fgenCurves.str(i)); 
	    }

	    // set output stream
	    PrintStream out = System.out;
	    if (outFimFile != null)
	        out = new PrintStream(
		    new FileOutputStream(outFimFile), true);

	    // write project
	    if (outProj) {
	    	proj.writeXML(out);
		return;
	    }

	    // initialize log
	    if (logFile != null)
	    	logwrt = new PrintWriter(logFile);

	    // start run MOPT generator thread
	    msg("Starting optimizations ...");
	    if (fmtName.equals("i4bull") || fitFmtName.equals("i4bull")) 
	        pfim.checkI4Bull();
	    ASInfo.Mopt jobInfo = pfim.makeJobInfo();
	    pjob = new PModelMoptJob(pfim.pmodel(), jobInfo);
	    pjob.setSaveOptimResults(logFile != null);
	    pjob.setNoAbort(noabort);
	    pjob.setCalcCovMat(conflim);
	    pjob.start();

	    // cancel timer
	    if (cancelDelay >= 0) {
	        System.err.println("Cancel timer started");
	    	Timer ctimer = new Timer(cancelDelay, this);
		ctimer.start();
	    }
	    
	    // monitor pjob to completion
	    while(pjob.stat() < 0) {
	    	try { Thread.sleep(500); } catch (Exception e) { }
		processSegments();
	    }
	    String stat = "abnormal " + pjob.stat();
	    if (pjob.stat() == PJob.NORMAL) 
	        stat = "normal";
	    msg("Job termination status=" + stat);
	    processSegments();

    	    // write FIM output
	    DataWriter wrt = fimFmt.createWriter();
	    wrt.setPrecision(outPrec);
	    Data.List list = moptData.parData();
	    if (outRMS) list.add(moptData.rmsData());
	    if (fmtName.equals("i4bull"))
	    	list = pfim.createI4BullData();
	    wrt.writeData(out, list);
	    out.close();
	    
	    // write fit output
	    if (outFitFile != null) {
		wrt = fitFmt.createWriter();
		wrt.setPrecision(outPrec);
	    	Data.List fitData = moptData.fitData();
	    	if (fmtName.equals("i4bull"))
	    	    fitData = pfim.createI4FitData();
	        out = new PrintStream(
		    new FileOutputStream(outFitFile), true);
		wrt.writeData(out, fitData);
		out.close();
	    }

	    // close log
	    if (logwrt != null) logwrt.close();
	    appl.server().disconnect();
	}

	// process all segments
	private void processSegments() throws Xcept {
	    if (moptData == null) moptData = pfim.moptData();
	    if (moptData == null) return;
	    if (segProc == null)
		segProc = new boolean[moptData.nsegments()];
	    boolean showTime = false;
	    for (int s=0; s<segProc.length; s++) {
		if (!segProc[s] && moptData.segmentDone(s)) {
		    processSegment(s);
		    segProc[s] = true;
		    showTime = true;
		}
	    }
	    if (showTime) 
		showTimeRemaining();
	}
	    	
	// progress message, log for  1 segment
	private void processSegment(int s) throws Xcept {
	    String name = segmentName(s);
	    StringBuffer buf = new StringBuffer("=== " + name + " ");
	    for (int p=0; p<moptData.npars(); p++) {
	    	Data data = moptData.parData(p);
		buf.append(" " + data.desc() + 
		    "=" + (float) data.realVal(s));
	    }
	    msg(buf.toString());

	    // log optim results
	    if (logwrt != null) {
		OptimResults r = moptData.optimResults(s);
		if (r != null) {
		    OptimReport rpt = new OptimReport(
		    	r, appl.server().optimAlgs());
		    logwrt.println(rpt.getReport());
		}
		logwrt.flush();
	    }
	}

	// print message
	private void msg(String s) {
	    if (!silent)
	    	System.err.println(s);
	    if (logwrt != null)
	    	logwrt.println(s);
	}


	// time remaining message, if available
	private void showTimeRemaining() {
	    if (silent) return;
	    if (pjob == null) return;
	    long trem = pjob.remainingTime();
	    if (trem <= 0) return; 
	    System.err.println("  est. finish in "
		+ Util.timeDiffString(trem));
	}

	// segment name for user
	private String segmentName(int s) throws Xcept {
	    String str = "Segment " + (s+1) + ": ";
	    Data data = pfim.matchData(s, 0);
	    if (data != null) str = str + data.legend();
	    return str;
	}

	// timer event
	public void actionPerformed(ActionEvent e) {
	    ((Timer) e.getSource()).stop();
	    System.err.println("Cancelling job now");
	    pjob.cancel();
	}
		
	// parse command line args
	public void parseArgs(String[] args) throws Xcept {
	    inputs = new StringList();
	    fgenVars = new StringList();
	    fgenCurves = new StringList();
	    fgenFiles = new JSReadable.List();
	    prefixes = new StringList();
	    excludes = new StringList();
	    outRMS = true;
	    int i = 0;
	    
	    // switches
	    while (i<args.length && args[i].charAt(0) == '-') {
	    	String s = args[i++];
		if (s.equals("-model") && i<args.length-1) 
		    modelName = args[i++];
		else if (s.equals("-i")) {
		    while (i<args.length-1 
		    && ! args[i].startsWith("-")
		    && args[i].indexOf('=') > 0)
		    	inputs.add(args[i++]);
		} else if (s.equals("-fgen") && i<args.length-1) {
		    String eqn = args[i++];
		    String fileName = null;
		    if (eqn.indexOf('=')<0 && i<args.length-1) {
		    	fileName = eqn;
		    	eqn = args[i++];
		    }
		    parseFgen(eqn, fileName);
		} else if (s.equals("-o") && i<args.length-1)
		    outFimFile = new File(args[i++]);
		else if (s.equals("-ofit") && i<args.length-1)
		    outFitFile = new File(args[i++]);
		else if (s.equals("-log") && i<args.length-1)
		    logFile = new File(args[i++]);
		else if (s.equals("-ofmt") && i<args.length-1)
		    fmtName = args[i++];
		else if (s.equals("-ofitfmt") && i<args.length-1)
		    fitFmtName = args[i++];
		else if (s.equals("-oprec") && i<args.length-1)
		    outPrec = Util.toInt(args[i++]);
		else if (s.equals("-ref") && i<args.length-1)
		    prefixes.add(args[i++]);
		else if (s.equals("-refx") && i<args.length-1)
		    excludes.add(args[i++]);
		else if (s.equals("-server") && i<args.length-1)
		    serverName = args[i++];
		else if (s.equals("-builddirsfx") && i<args.length-1)
		    buildDirSfx = args[i++];
		else if (s.equals("-cancel") && i<args.length-1)
		    cancelDelay = Util.toInt(args[i++]);
		else if (s.equals("-oproj"))
		    outProj = true;
		else if (s.equals("-noabort"))
		    noabort = true;
		else if (s.equals("-conflim"))
		    conflim = true;
		else if (s.equals("-silent"))
		    silent = true;
		else if (s.equals("-usage")) {
		    usage = true;
		    return;
		} else if (s.equals("-mp") && i<args.length-1)
		    maxProc = Util.toInt(args[i++]);
		else if (s.equals("-mpdebug") && i<args.length-1)
		    maxProcDebug = Util.toInt(args[i++]);
		else 
		    abort("Unrecognized switch " + s);
	    }

	    // last 2 args are project match file
	    int n = args.length - i;
	    if (n != 2) abort(
	    	"2 arguments required, " + n + " found");
	    projFile = new JSReadable(args[i++]);
	    matchFile = new JSReadable(args[i++]);
	}
		
	// parse fgens
	public void parseFgen(String eqn, String fileName) throws Xcept {
	    int inx = eqn.indexOf('=');
	    if (inx < 0) abort("Expected v=curve,  found " + eqn);
	    fgenVars.add(eqn.substring(0,inx));
	    fgenCurves.add(eqn.substring(inx+1));
	    fgenFiles.add(
	    	(fileName == null) ? null : new JSReadable(fileName));
	}

	// abort due to command line error
	public void abort(String s) throws Xcept {
	    throw new Xcept("jsfim usage error: " + s);
	}
		
	// usage summary
	public void usage() throws Xcept {
	    String res = "jsfimUsage.txt";
	    URL url = getClass().getResource(res);
	    if (url == null) throw new Xcept(
		"Resource \"" + res + "\" not found");
	    String txt = UtilIO.readText(url);
	    System.err.println(txt);
	}

	// mainline
	public static void main(String[] args) throws Exception {
	    new FimMain(args);
	}
}
