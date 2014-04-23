/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test harness for Plan

package JSim.tests; import JSim.plan.*; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.jcode.*;

import java.io.*;
import java.util.*;
import org.w3c.dom.*;

public class PlanTest {
	boolean quiet = false;
	boolean lineFmt = false;
	File logDir = null;
	boolean outXMML = false;
	String outGML = null;
	boolean outJava = false;
	boolean doTime = false;
	String vtags;
	NamedVal.NList options;
	int exitStat = 0;

	// for 1 model
	PrintStream out; // log stream
	String modFileName;
	Model mmlModel;
	Plan plan;
	String modStat, boxStat, seqStat, msgStat;

	// constructor
	public PlanTest(String[] args) throws Exception {
	    out = System.out;

	    // logging switches
	    int i=0;
	    vtags = null;
	    while (i<args.length && args[i].startsWith("-")) {
	    	String arg = args[i++];
		if (arg.startsWith("-v"))
		    vtags = arg.substring(2);
		else if (arg.equals("-q"))
		    quiet = true;
		else if (arg.equals("-t"))
		    doTime = true;
		else if (arg.equals("-ox"))
		    outXMML = true;
		else if (arg.startsWith("-og"))
		    outGML = arg.substring(3);
		else if (arg.equals("-oj"))
		    outJava = true;
		else if (arg.equals("-line"))
		    lineFmt = true;
		else if (arg.equals("-log"))
		    logDir = new File(args[i++]);
		else throw new Xcept(
		    "Unknown switch: " + arg);
	    }

	    // init logDir
	    if (logDir != null) {
	    	if (! logDir.exists())
	    	    logDir.mkdir();
	    	if (! logDir.isDirectory()) throw new Xcept(
	    	    "Can't create log directory=" + logDir);
	    }

	    // compile options
	    options = new NamedVal.NList();
	    while (i<args.length && args[i].indexOf('=') > 0) {
	    	int inx = args[i].indexOf('=');
		String name = args[i].substring(0, inx);
		String value = args[i].substring(inx+1);
	    	NamedVal nval = NamedVal.guess(name, value);
		options.add(nval);
		i++;
	    }

	    // loop over models
	    int nmodels = args.length - i;
	    for(; i<args.length; i++) {
	    	modFileName = args[i];
		if (! modFileName.endsWith(".mod")) throw new Xcept(
		    "Illegal mod file name: " + modFileName);

		// log file?
		if (logDir != null) {
		    File modFile = new File(modFileName);
		    String modName = modFile.getName();
		    String logFileName = modName.substring(0, modName.length()-4)
		    	+ ".out";
		    File logFile = new File(logDir, logFileName);
		    OutputStream logOut = new FileOutputStream(logFile);
		    out = new PrintStream(logOut, true);
		}

		boolean strictOut = outXMML || (outGML != null) || outJava;
	        if (! strictOut && ! lineFmt) 
		    out.println("\n==== " + modFileName);
	 	if (strictOut && nmodels > 1) 
		    System.err.println("\n==== " + modFileName);
		mmlModel = null;
		modStat = "FAIL";
		boxStat = "SKIP";
		seqStat = "SKIP";
		msgStat = "";
		plan = null;
		try {
	    	    mmlModel = new ModelReader(new File(modFileName));
	    	    mmlModel.flatten("testmodel.junk");
		    modStat = "PASS";
		} catch (Exception e) {
		    if (!lineFmt)
		    	out.println("ERROR: MML read failed");
		    exitStat = 1;
		}
		long time = System.currentTimeMillis();
		try {
		    makePlan();
		} catch (Exception e) {
		    process(e);
		}
		time = System.currentTimeMillis() - time;

		// special output types
	    	if (outXMML) {
		    XMMLWriter xmml = new XMMLWriter(plan);
		    xmml.write(out);
	    	}
	    	if (outGML != null) try {
	    	    XMLWriter wrt = new XMLWriter();
		    Document doc = plan.getGraphML();
		    if (! Util.isBlank(outGML)) {
		    	Element root = doc.getDocumentElement();
		    	UtilXML.removeElementsExceptID(root, outGML);
		    }
		    wrt.write(doc, out); 
	    	} catch (Xcept e) {
		    process(e);
	    	}   
	    	if (outJava) try {
	    	    JPlanWriter wrt = new JPlanWriter("JTest", plan);
	 	    wrt.setSourceLibs();
		    wrt.writePlan(); 
	    	} catch (Exception e) {
		    process(e);
	    	}   
		if (lineFmt) {
		    if (plan != null) {
		    	String stat = (plan.logger.nerrors() > 0) ?
		    	    "FAIL" : "PASS";
		    	if (plan.main() == null) { 
			    boxStat = stat;
		    	} else {
			    boxStat = "PASS";
			    seqStat = stat;
			}
		    	msgStat = plan.logger.errors.toString().replaceAll("\n", ";");
		    }
		    String line = modFileName +
		    	" MOD=" + modStat +
		    	" BOX=" + boxStat +
		    	" SEQ=" + seqStat;
	 	    if (doTime) line = line + " time=" + time;
		    line = line + " MSG=" + msgStat;
		    out.println(line);
		} else {
		    if (doTime)
		    	System.err.println("  elapsed time=" + time);
		}
	    }

	    System.exit(exitStat);
	}


	// make plan
	private void makePlan() throws Exception {
	    if (mmlModel == null) throw new Xcept(
	        "MML parsing failed");
	    plan = new Plan(mmlModel);
	    plan.setVerbose(vtags);
	    plan.setOptions(options);
	    if (! lineFmt)
	        plan.logger.setStream(out);
	    plan.process();
	}

	// process exception
	private void process(Exception e) {
	    if (! quiet) e.printStackTrace();
	    exitStat = 1;
	}

	// mainline
	public static void main(String[] args) throws Exception {
	    new PlanTest(args);
	}
}
