/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// batch jsim interface

package JSim.text;

import java.io.*;	// File and PrintStream
import java.util.ArrayList;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 
import JSim.project.*;
import JSim.xsim.*;

public class Batch {
	static BAppl appl;  	// text application
	static boolean stackTrace; // private copy of trace stack

	PrintStream out; // output file or stream
	Project project; // project loaded
	Data.List outdata; // data for output
	PModel pmodel; 	// model to process
	ArrayList<ASQuery> outputExpr; // exprs to output
	BatchQuery queryTest; // async query test

	// mainline
  	public static final void main(String[] args) {
    	    try {
	        new Batch(args);
		appl.server().disconnect(); 
		System.exit(0);
	    } catch (Exception e) {
		printException(e);
	    	if (appl != null && appl.server() != null)
		    appl.server().disconnect(); 
		System.exit(1);
	    }
	}
	
	// constructor 
	public Batch(String[] args) throws Exception {
	    stackTrace = true;
	    appl = new BAppl(args);
	    mainline();
	}
	    	
	// mainline
	private void mainline() throws Exception {
		
	    // set up traces
	    stackTrace = appl.stackTrace;
	    out = System.out;
	    if (appl.outFile != null)
		out = new PrintStream(
		    new FileOutputStream(appl.outFile));

	    // create project
	    project = new Project("proj", appl);
	    if (appl.projFile != null) {
		project.importXML(appl.projFile);
		// project.buildStable();
	    }

	    // import content from other projects
	    for (int i=0; i<appl.projImports.size(); i++) 
		projImport(appl.projImports.str(i));

	    // load files into project
	    for (int i=0; i<appl.projFileLoads.size(); i++) 
		project.load(appl.projFileLoads.readable(i), null);

	    // output named parset?
	    if (appl.outputParset != null) {
	    	if (! (project.child(appl.outputParset) instanceof ParSet))
		    throw new Xcept("No such parset: " + appl.outputParset);
		ParSet p = (ParSet) project.child(appl.outputParset);
		p.writeFile(out);
		return;
	    }

	    // load model to process
	    if (appl.modelStr == null) {
		for (int i=0; i<project.nChild(); i++) {
		    if (project.child(i) instanceof PModel) {
			pmodel = (PModel) project.child(i);
			break;
		    }
		}
	    } else {
		PNamed p = project.child(appl.modelStr);
		if (! (p instanceof PModel)) throw new Xcept(
		    "Model <" + appl.modelStr + "> not found");
		pmodel = (PModel) p;
	    }
	    Util.verbose("Processing model " + pmodel);

	    // load images
	    for (int i=0; i<appl.imageStr.size(); i++) {
		if (pmodel == null) throw new Xcept(
		    "No model to load images into.");
		String fname = appl.imageStr.str(i);
		int inx = fname.lastIndexOf('.');
		String name = (inx<1) ? fname : fname.substring(0, inx);
		PImage pimg = new PImage(pmodel.images(), name);
	 	pimg.load(new File(fname));
	    }
		    
	    // import model RTML
	    if (appl.modelRTML != null) {
		if (pmodel == null) throw new Xcept(
		    "No model to load RTML into.");
		File f = new File(appl.modelRTML);
		pmodel.customRTML().importRTML(f);
		pmodel.customRTML().rtml(); // error check
	    }

	    // create func gens
	    for (int i=0; i<appl.funcStr.size(); i++) {
		if (pmodel == null) throw new Xcept(
		    "No model to create funcgen for.");
		String name = appl.funcStr.str(i);
		new FuncGen(pmodel.vars(), name);
	    }

	    // create plots, nesteds
	    for (int i=0; i<appl.plotStr.size(); i++) 
		new PlotPage(project, appl.plotStr.str(i));
	    for (int i=0; i<appl.nestedStr.size(); i++) 
		new PNested(project, appl.nestedStr.str(i));

	    // remove some items
	    for (int i=0; i<appl.rmStr.size(); i++) {
		String n = appl.rmStr.str(i);
		boolean stat = project.remove(n);
		if (!stat) throw new Xcept(project, 
		    "Could not remove project child " + n);
	    }

	    // load model source
	    if (appl.loadModel != null) {
		JSReadable r = new JSReadable(appl.loadModel);
	    	if (pmodel == null) throw new Xcept(r, 
		    "No model to load source into");
		String src = appl.readSource(r, null);
		pmodel.setSource(r, src);
	    }

	    // text output may not require compilation
	    if (appl.outputText >= 0) {
	        if (pmodel == null) 
	            throw new Xcept("No model to output text for.");
	    	int sourceType = pmodel.sourceType.val();

		// source matches requested output
		if (appl.outputText == sourceType) {
	    	    out.println(pmodel.modelSource.val());
		    return;
		}

		// Java source can't be translated to anything else
	    	if (sourceType == ASModel.TEXT_JAVA) 
	    	    throw new Xcept(
		        ASModel.TEXT_NAMES[appl.outputText] +
			" text not available from Java model source");
	    }

	    // output RTML?
	    if (appl.outputRTML) {
		if (pmodel == null) throw new Xcept(
		    "No model to output RTML for.");
		pmodel.customRTML().exportRTML(out);
		return;
	    }

	    // build model
	    if (pmodel != null) {
	    	pmodel.options().setControls(appl.buildOptions);
		PJob pjob = new PModelBuildJob(pmodel);
		pjob.setStackTrace(stackTrace);
		pjob.simpleRun();
		project.revalidate();
		printWarnings(pmodel.rt().getBuildAlerts());
	    }

	    // output other model text?
	    if (appl.outputText>0) {
		if (pmodel == null) throw new Xcept(
		    "No model to output text for.");
		String text = pmodel.rt().getText(
		    appl.outputText, appl.outputTextVariant);
		out.println(text);
		printWarnings(pmodel.rt().getTextWarnings(
		    appl.outputText, appl.outputTextVariant));
		return;
	    }

	    // load parset?
	    if (appl.loadParSet != null) {
		if (pmodel == null) throw new Xcept(
		    "No model to load parset into.");
		pmodel.loadParSet(appl.loadParSet);
	    }
	    if (appl.loadXSParFile != null) {
		if (pmodel == null) throw new Xcept(
		    "No model to load XSIM parfile into.");
		File f= new File(appl.loadXSParFile);
		XSParFile xspars = new XSParFile(f);
		xspars.assignModelPars(pmodel);
		StringList report = xspars.report();
		for (int j=0; j<report.size(); j++)
		    Util.verbose(report.str(j));
	    }

	    // load program assigned inputs, parse outputs
	    parseInputs();
	    parseOutputs();

	    // query mode, exit
	    if (appl.query) {
		if (pmodel == null) throw new Xcept(
		    "No model to query.");
	    	VarQuery vq = new VarQuery(pmodel.rt());
		String text = vq.getText(appl.queryStr);
		out.println(text);
		return;
 	    }

	    // dataset filtering
	    for (int i=0; i<appl.filterDataSets.size(); i++) {
		String n = appl.filterDataSets.str(i);
		PNamed pnamed = project.nestedChild(n);
		if (! (pnamed instanceof PDataSet)) throw new Xcept(
		    n + " is not a project data set");
		PDataSet dataset = (PDataSet) pnamed;
		dataset.filterData();
	    }

	    // output model parset, if no optim?
	    if (appl.outputModelParset && ! appl.optim) {
	    	pmodel.lastParSet().writeFile(out);
		return;
	    }
		
	    // run model?
	    boolean dorun = true;
	    if (appl.outputProj) {
		dorun = false;
		if (appl.optim) dorun = true;
		if (appl.storePDataSet != null) dorun = true;
		if (appl.fwdIC) dorun = true;
	    }
	    if (appl.outputDataset != null) 
		dorun = appl.storePDataSet != null;
	    if (appl.outputFgenPreview) dorun = false;
	    if (dorun && pmodel != null) runModel();

	    // store par set?
	    if (appl.storeParSet != null) 
		pmodel.storeParSet(appl.storeParSet);

	    // final var output
	    if (appl.outputFinal) {
	    	writeFinal();
		return;
	    }

	    // calculate output data 
	    if (appl.outputFgenPreview)
		calcFgenPreviewData();
	    else if (appl.outputPlots) 
		calcPlotData();
	    else if (appl.outputNested) 
		calcNestedData();
	    else if (appl.outputDataset != null)
		calcDataSetData();
	    else if (appl.monte)
	    	calcMonteData();
	    else if (appl.storePDataSet != null || ! appl.outputProj)
		calcModelData();

	    // store output in PDataSet?
	    if (appl.storePDataSet != null) {
		String n = project.newChildName(
		    appl.storePDataSet, false);
		PDataSet dataset = new PDataSet(project, n);
		    dataset.importData(outdata);
	    }

	    // write project or data output
	    if (appl.outputProj) 
		writeProj();	
	    if (appl.outputReport) 
		writeReport();	
	    else if (appl.outputProfile) 
		writeProfile();	
	    else
 		writeOutData();
	}

	// import project content
	private void projImport(String s) throws Xcept {
	    int inx = s.lastIndexOf(':');
	    String sproj = (inx<0) ? s : s.substring(0,inx);
	    JSReadable projFile = new JSReadable(sproj);
	    String baseName = projFile.fileBaseName();
	    Project nproj = new Project(baseName, appl);
	    nproj.importXML(projFile);
	    StringList items = new StringList(4);
	    if (inx>=0) 
	    	items.add(s.substring(inx+1));
	    else 
	    	for (int i=0; i<nproj.nChild(); i++)
		    items.add(nproj.child(i).name());
	    for (int i=0; i<items.size(); i++)
	    	projImport(nproj, items.str(i));
	}
	
	// import a single project child
	private void projImport(Project nproj, String name)
	throws Xcept {
	    Util.verbose("Importing project " + nproj.name() + 
	    	" content " + name);
	    PNamed pnamed = project.child(name);
	    if (pnamed != null) {
	    	Util.verbose("  Removing previous content " + name);
		project.remove(pnamed);
	    }
	    pnamed = nproj.child(name);
	    if (pnamed == null) throw new Xcept(nproj,
	    	"Project content " + name + " not found");
	    Element e = pnamed.exportXML();
	    project.importXMLChild(e);
	}

	// parse inputStr
	private void parseInputs() throws Xcept {
	    for (int i=0; i<appl.inputStr.size(); i++) {
		String s = appl.inputStr.str(i);
		int inx = s.indexOf('=');
		if (inx < 1) throw new Xcept(s +
		    ": Input variable assignment required (v=expr)");
	    	String cname = s.substring(0, inx);
		String exprstr = s.substring(inx+1);
		PNamed pnamed = project.nestedChild(cname);
		if (pnamed == null)
		    pnamed = pmodel.vars().control(cname);
//		if (pnamed == null)
//		    pnamed = pmodel.nestedChild(cname);
		if (pnamed == null) throw new Xcept(
		    cname + ": no such control name");
		if (! (pnamed instanceof Control)) throw new Xcept(
		    cname + ": this is not a settable control");
		Control cntl = (Control) pnamed;
		cntl.setVal(exprstr);
		if (! (cntl instanceof StringControl)) continue;
		StringControl scntl = (StringControl) cntl;
		if (!scntl.valid()) throw new Xcept(
		    s + " : " + scntl.validMsg());
	    }
	}

	// parse outputStr
	private void parseOutputs() throws Xcept {
	    if (pmodel == null) return;
	    if (appl.monte) return;
	    outputExpr = new ArrayList<ASQuery>();
	    if (appl.outputStr == null) {
	    	ASVar.List asvars = pmodel.rt().getASVars();
		for (int i=0; i<asvars.size(); i++)
		    outputExpr.add(asvars.asvar(i));
	    } else {
	    	for (int i=0; i<appl.outputStr.size(); i++) {
		    String s = appl.outputStr.str(i);
		    ASQuery e = pmodel.rt().parseQuery(s);
		    outputExpr.add(e);
	        }
	    }
	}

	// run a model
	private void runModel() throws Exception {
	    // start QueryTest,  if requested
	    if (appl.queryTestExpr != null) {
		queryTest = new BatchQuery(pmodel,
		    appl.queryTestPause, appl.queryTestExpr);
		queryTest.start();
	    }

	    long tstart = System.currentTimeMillis();

/*	    // run optimizer first?
	    if (appl.optim) {
		Util.verbose("==== Optimizing model " + pmodel.name());
	    	PJob pjob = new PModelOptimJob(pmodel);
	    	pjob.setStackTrace(stackTrace);
	    	pjob.simpleRun();
		if (appl.outputReport) {
		    OptimResults r = pmodel.rt().optimResults();
		    String s = "null OptimResults";
		    if (r != null) {
		    	OptimReport rpt = new OptimReport(
			     r, appl.server().optimAlgs());
			s = rpt.getReport();
		    }
		    out.println(s);
		}
	    }
*/
	    // run model (various job types)
	    for (int j=0; j<appl.nruns; j++) {
	    	Util.verbose("==== Running model " + pmodel.name() +
		    ((appl.nruns>1) ? (" pass #" + (j+1)) : ""));
	    	PJob pjob = null;
	 	if (appl.loops)  
		    pjob = new PModelLoopsJob(pmodel);
	 	else if (appl.sens) 
		    pjob = new PModelSensJob(pmodel);
		else if (appl.optim) 
		    pjob = new PModelOptimJob(pmodel);
		else if (appl.monte) 
		    pjob = pmodel.monte().createJob();
		else
		    pjob = new PModelRunJob(pmodel);
	    	pjob.setStackTrace(stackTrace);
		setSaveExprs(pjob);
	    	try {
	    	    pjob.simpleRun();
	    	} catch (Exception e) {
		    if (appl.outputCrash) 
		    	printException(e);
		    else
		    	throw e;
	    	}
	    	if (appl.fwdIC) pmodel.forwardICs(appl.fwdICDoms); // forward ICs
	    }

	    // run time
	    if (appl.timing) {
	        long tdelta = System.currentTimeMillis() - tstart;
	    	System.err.println("Model " + pmodel.name() +
		   ": run time=" + tdelta + " msec");
	    }   

	    // terminate queryTest, if any
	    if (queryTest != null)
		queryTest.terminate();
	}

	// set saveExprs in job
	private void setSaveExprs(PJob pjob) throws Xcept {
	    int scheme = pjob.baseVals().intVal(
	    	"memory.storeScheme", ASModel.MEMORY_STATIC);
	    if (scheme != ASModel.MEMORY_SELECTED) return;
	    StringList list = new StringList();
	    if (appl.outputPlots || appl.outputNested) {
	    	project.addModelExprs(pmodel, list);
	    } else if (outputExpr != null) {
	    	for (int i=0; i<outputExpr.size(); i++)
		    list.add(outputExpr.get(i).toString());
	    }
	    pjob.setSaveExprs(list);
	}


	// query dataset
	private void queryPDataSet(PDataSet dataset)
	throws Xcept {
	    for (int i=0; i<dataset.nData(); i++) {
		Data data = dataset.data(i);
		out.println("\t" +
		    ((data.name()==null) ? "<>" : data.name()) + 
		    "\t" + data.desc() + "\t" + data.unit());
	    }
	}

	// calculate fgen preview data
	private void calcFgenPreviewData() throws Xcept {
	    outdata = new Data.List(outputExpr.size());
	    PNamed.List list = project.descendants(FuncGen.class);
	    for (int j=0; j<list.size(); j++) {
		FuncGen fgen = (FuncGen) list.pnamed(j);
		Data data = fgen.previewData();
		outdata.add(data);
	    }
	}

	// calculate output data from Monte-Carlo results
	private void calcMonteData() throws Xcept {
	    outdata = new Data.List(1);
	    if (appl.outputReport) return;
	    MoptData moptData = pmodel.rt().getMoptData();
	    outdata = moptData.parData();
	}

	// calculate output data from Model
	private void calcModelData() throws Xcept {
	    int ct = (outputExpr != null) ? outputExpr.size() : 0;
	    outdata = new Data.List(ct);
	    if (appl.outputReport) return;
	    for (int i=0; i<ct; i++) {
		ASQuery expr = outputExpr.get(i);
		ASModel rt = pmodel.rt();
	    	for (int run=0; run<pmodel.nQueryStores(); run++) {
		    Data data = rt.getData(run, expr);
		    outdata.add(data);
	    	}
	    }
	}

	// calculate output data from Plots
	private void calcPlotData() throws Xcept {
	    outdata = new Data.List(8);
	    for (int i=0; i<project.nChild(); i++) {
		if (! (project.child(i) instanceof PlotPage)) 
		    continue;
		PlotPage page = (PlotPage) project.child(i);
		page.addData(outdata);
	    }
	}

	// calculate nested data
	private void calcNestedData() throws Xcept {
	    outdata = new Data.List();
	    for (int i=0; i<project.nChild(); i++) {
		if (! (project.child(i) instanceof PNested)) 
		    continue;
		PNested pnested = (PNested) project.child(i);
		PNestedData pdata = new PNestedData(pnested);
		Data.List dlist = pdata.getGraphData();
		outdata.addAll(dlist);
	    }
	}

	// load output data from DataSet
	private void calcDataSetData() throws Xcept {
	    PNamed pnamed = project.child(appl.outputDataset);
	    if (! (pnamed instanceof PDataSet)) throw new Xcept(
	    	"Dataset not found: " + appl.outputDataset); 
	    PDataSet dataset = (PDataSet) pnamed;
	    outdata = dataset.dataList();
	}

	// write project content
	public void writeProj() throws Xcept {
	    Util.verbose("Writing project content...");
	    if (appl.projExports.size() == 0) {
	    	project.writeXML(out);
		return;
	    }
	    Project nproj = new Project("proj2", appl);
	    for (int i=0; i<appl.projExports.size(); i++) {
	    	String name = appl.projExports.str(i);
	    	PNamed pnamed = project.child(name);
		if (pnamed == null) throw new Xcept(
		    "Specified project content output " + name +
		    " does not exist");
		Element e = pnamed.exportXML();
	        nproj.importXMLChild(e);
	    }
	    nproj.writeXML(out);
	}

	// write output data
	public void writeOutData() throws Xcept {
	    if (outdata == null) return;
	    Util.verbose("Writing output data...");

	    // prune to correct dimension
	    Data.List list = outdata;
	    if (appl.outputDim >= 0) {
		list = new Data.List(outdata.size());
		for (int i=0; i<outdata.size(); i++) {
		    Data data = outdata.data(i);
		    if (data instanceof GridData) continue;
		    if (data.ndim() != appl.outputDim) 
			continue;
		    list.add(data);
		}
	    	if (list.size() == 0) throw new Xcept(
	    	    "No " + appl.outputDim + "-D data is available");
	    }

	    // write to required format
	    DataFormat fmt = appl.dataFormats().format(
		appl.formatStr);
	    DataWriter wrt = fmt.createWriter();
	    wrt.setPrecision(appl.precision);
	    wrt.setZeroThresh(appl.zeroThresh);
	    wrt.setEncoding(appl.outputEncoding);
	    if (appl.outputBlocked) {
		if (wrt instanceof DataWriter.Blocked) {
		    DataWriter.Blocked bwrt = (DataWriter.Blocked) wrt;
		    bwrt.setBlocks(list);
		    bwrt.writeData(out, appl.outputBlockMin, appl.outputBlockMax);
		} else {
		    throw new Xcept(fmt, "Data format does not support blocked output.");
		}
	    } else {
	    	wrt.writeData(out, list);
	    }
	    if (wrt.warning() != null) 
		System.err.println("Warning: " + wrt.warning());
	}

	// write final variable values
	private void writeFinal() throws Xcept {
	    ASVar.List asvars;
	    if (appl.outputStr.size() == 0) {
	    	asvars = pmodel.rt().getASVars();
	    } else {
	    	asvars = new ASVar.List(appl.outputStr.size());
	    	for (int i=0; i<appl.outputStr.size(); i++) {
		    String s = appl.outputStr.str(i);
		    ASVar v = pmodel.rt().getASVar(s);
		    asvars.add(v);
	        }
	    }
	    for (int i=0; i<asvars.size(); i++) {
	    	ASVar v = asvars.asvar(i);
		double f = v.finalRealVal();
		out.println(v.name() + "\t" + Util.pretty(f));
	    }
	}

	// write Optimization or Monte-Carlo report
	private void writeReport() throws Xcept {

	    // Optimization report
	    if (appl.optim) {
	        writeReport(pmodel.rt().optimResults());
	    } 
	    
	    // Monte-Carlo report
	    if (appl.monte) {
	    	MoptData moptData = pmodel.rt().getMoptData();
	    	PModelMonteReport rpt = 
		    new PModelMonteReport(pmodel.monte(), moptData);
	    	out.println(rpt.getReport());
		if (appl.outputReports) {
		    for (int i=0; i<moptData.nsegments(); i++) {
		    	out.println("==== Optimization #" + (i+1));
		    	writeReport(moptData.optimResults(i));
		    }
		}
	    } 
	}

	// write optimization report
	private void writeReport(OptimResults res) throws Xcept {
	    OptimReport rpt = new OptimReport(
		res, appl.server().optimAlgs());
	    out.println(rpt.getReport());
        }

	// write profile report
	private void writeProfile() throws Xcept {
	    ProfileData pdata = pmodel.rt().getProfile();
	    ProfileReport prpt = new ProfileReport(pdata);
	    String text = prpt.getReport();
	    out.println(text);
	}
		
	// print Exception
	private static void printException(Exception e) {
	    System.err.println("==== JSim version " +
		Util.version() + ": Fatal Error\n");
	    if (stackTrace) e.printStackTrace();
	    else System.err.println(e.toString());
	}

	// print warning array
	private void printWarnings(String[] msgs) {
	    if (msgs == null) return;
	    for (int i=0; i<msgs.length; i++) 
	    	System.err.println(msgs[i]);
	}


}

