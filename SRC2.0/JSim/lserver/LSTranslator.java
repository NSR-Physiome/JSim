/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Translate model code from one language to another: version 2.0

package JSim.lserver;


import java.io.*;
import java.util.Hashtable;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import JSim.aserver.*;
import JSim.util.*;
import JSim.data.*;
import JSim.cellml.*;
import JSim.sbml.*;
import JSim.mml.ModelReader;

public class LSTranslator {
	private LSServer server;
	private int srcType;
	private String srcText;
	private Document srcDoc; // for XML types, if needed

	// latest output only
	private StringList warnings;

	// constructor
	public LSTranslator(LSServer server, int srcType, 
	String srcText) throws Xcept {
	    this.server = server;
	    this.srcText = srcText;
	    this.srcType = srcType;

	    // disambiguate srcType=XML to CELLML or SBML
	    if (srcType == ASModel.TEXT_XML) {
		srcDoc = UtilXML.parse(srcText);
		String type = srcDoc.getDocumentElement().getNodeName();
		if (type.equals("sbml"))
		    this.srcType = ASModel.TEXT_SBML;
		else if (type.equals("model"))
		    this.srcType = ASModel.TEXT_CELLML;
		else throw new Xcept("Unsupported XML file format: "
		    + " document element=" + type);
	    }		    
	}
	
	// translate text
	public String getText(int destType, String options)
	throws Xcept {
	    warnings = new StringList();
	    if (srcType == ASModel.TEXT_SBML && destType == ASModel.TEXT_MML)
	    	return sbml2MML(srcText, options);
	    if (srcType == ASModel.TEXT_CELLML && destType == ASModel.TEXT_MML)
	    	return cellml2MML(srcText, options);
	    if (srcType == ASModel.TEXT_ANTIMONY && destType == ASModel.TEXT_MML)
	    	return antimony2MML(srcText, options);
	    if (srcType == ASModel.TEXT_XMML && destType == ASModel.TEXT_SBML)
	    	return xmml2SBML(srcText, options);
	    if (srcType == ASModel.TEXT_XMML && destType == ASModel.TEXT_CELLML)
	    	return xmml2CellML(srcText, options);
	    if (srcType == ASModel.TEXT_SBML && destType == ASModel.TEXT_MATLAB)
	    	return sbml2Matlab(srcText, options);
	    if (srcType == ASModel.TEXT_MML && destType == ASModel.TEXT_XMML)
	    	return mml2XMML(srcText, options);
	    if (srcType == ASModel.TEXT_SBML && destType == ASModel.TEXT_XMML)
	    	return sbml2XMML(srcText, options);
	    if (srcType == ASModel.TEXT_CELLML && destType == ASModel.TEXT_XMML)
	    	return cellml2XMML(srcText, options);
	    if (srcType == ASModel.TEXT_ANTIMONY && destType == ASModel.TEXT_XMML)
	    	return antimony2XMML(srcText, options);

	    if (canSBTranslate(srcType) && canSBTranslate(destType))
	    	return sbtranslate(srcType, destType, srcText, options);

	    throw new Xcept("Translation from " + 
	        ASModel.TEXT_NAMES[srcType] + " to " + 
	        ASModel.TEXT_NAMES[destType] + 
		" is not yet supported");		
	}

	// can sbtranslate handle this text type
	private boolean canSBTranslate(int type) {
	    if (type == ASModel.TEXT_SBML) return true;
	    if (type == ASModel.TEXT_ANTIMONY) return true;
	    if (type == ASModel.TEXT_CELLML) return true;
	    return false;
	}

	// SBML -> MML
	private String sbml2MML(String srcText, String options) throws Xcept {
	    SBModel sbmodel = new SBModel(srcText, 
	    	server.getCommonUnits("nsrunit.mod"), 
		options);
	    Writer wrt = new StringWriter();
	    sbmodel.writeMML(wrt);
	    return wrt.toString();
	}

	// CellML -> MML
	private String cellml2MML(String srcText, String options) throws Xcept {
	    if (srcDoc == null) srcDoc = UtilXML.parse(srcText);
	    CMLDoc cmlDoc = new CMLDoc(srcDoc, 
	    	server.getCommonUnits("cellmlunit.mod"),
		server.getCommonUnits("nsrunit.mod"));
	    Writer wrt = new StringWriter();
	    cmlDoc.writeMML(wrt);
	    return wrt.toString();
	}
	
	// Antimony -> MML (via SBML)
	private String antimony2MML(String srcText, String options) throws Xcept {
	    String sbmlText = sbtranslate(ASModel.TEXT_ANTIMONY, 
	    	ASModel.TEXT_SBML, srcText, options);
	    return sbml2MML(sbmlText, options);
	}

	// XMML -> SBML
	private String xmml2SBML(String srcText, String options) throws Xcept {
		ModelReader mr = new ModelReader(this.server.mmlText);
	    if (srcDoc == null) srcDoc = UtilXML.parse(srcText);
	    try {
			  	SBWriter swrt = new SBWriter(srcDoc, warnings,mr.getCommentsHT(), mr.getIdentifiersHT() );
				return swrt.getSBML(options);
			
	    } catch (Exception e) {
	     	throw Xcept.wrap(e);
	    }
	}

	// XMML -> CellML
	private String xmml2CellML(String srcText, String options) throws Xcept {
	    if (srcDoc == null) srcDoc = UtilXML.parse(srcText);
	    try {
	    	CMLWriter cwrt = new CMLWriter(srcDoc, warnings);
	    	Document doc = cwrt.getCellML(options);
		XMLWriter xwrt = new XMLWriter();
		StringWriter swrt = new StringWriter();
		xwrt.write(doc, swrt);
		return swrt.toString();
	    } catch (Exception e) {
	     	throw Xcept.wrap(e);
	    }
	}

	// call sbtranslate: SBML/CellML/Antimony all dirs
	private String sbml2Matlab(String srcText, String options) throws Xcept {
	    String exe = getHelperName("sbml2matlab");
	    File outFile = server.newWorkFile("translated", "m");
	    String[] cmdarr = new String[] { exe, "-output", 
	    	outFile.toString() };
	    return runHelperProg(cmdarr, srcText, outFile);
	}

	// MML -> XMML
	private String mml2XMML(String srcText, String options) throws Xcept {
	    File flatFile = new File(server.buildDir, "translator.flat");
	    NamedVal.NList poptions = new NamedVal.NList();
	    if (options != null && options.equals("sbml"))
	        poptions.setVal("makeDEICParms", false);
	    LSPlan plan = new LSPlan(server, srcText, flatFile, poptions);
	    plan.makePlan();
	    return plan.getXMMLText();
	}

	// SBML -> XMML
	private String sbml2XMML(String srcText, String options) throws Xcept {
	    String mml = sbml2MML(srcText, options);
	    return mml2XMML(mml, options);
	}

	// CellML -> XMML
	private String cellml2XMML(String srcText, String options) throws Xcept {
	    String mml = cellml2MML(srcText, options);
	    return mml2XMML(mml, options);
	}

	// Antimony -> XMML
	private String antimony2XMML(String srcText, String options) throws Xcept {
	    String mml = antimony2MML(srcText, options);
	    return mml2XMML(mml, options);
	}

	// call sbtranslate: SBML/CellML/Antimony all dirs
	private String sbtranslate(int srcType, int destType, String srcText,
	String options) throws Xcept {
	    String sbexe = getHelperName("sbtranslate");
	    File outFile = server.newWorkFile("translated", "txt");
	
	    // rest of command args
	    String outlang = null;
	    switch (destType) {
	    case ASModel.TEXT_SBML: outlang = "sbml"; break;
	    case ASModel.TEXT_ANTIMONY: outlang = "antimony"; break;
	    case ASModel.TEXT_CELLML: outlang = "cellml"; break;
	    }
	    String[] cmdarr = new String[] {
	    	sbexe, "-o", outlang, "-stdin", "-outfile", outFile.toString() };

	    return runHelperProg(cmdarr, srcText, outFile);
	}


	// run helper program
	private String getHelperName(String baseName) throws Xcept {
	    // name of sbtranslate (try .sh wrapper first)
	    String sbname = baseName + ".sh"; 
 	    String sbexe = null;
	    try { 
	        sbexe = UtilIO.pathFind(sbname,
	    	    System.getProperty("java.library.path"));
	    } catch (Xcept e) {
	        sbname = baseName;
 	    	if (Util.isWin32()) sbname = sbname + ".exe"; 
                sbexe = UtilIO.pathFind(sbname,
	    	    System.getProperty("java.library.path"));
	    }
	    return sbexe;
	}
	    
	// run helper program 
	private String runHelperProg(String[] cmdarr, String srcText,
	File outFile) throws Xcept {
	    try {
		String progName = cmdarr[0];
	    	Process proc = Runtime.getRuntime().exec(cmdarr);
		PrintStream send = new PrintStream(
		    proc.getOutputStream(), true);
		send.print(srcText);
		send.close();
		ProcessRunnable rproc = new ProcessRunnable(proc);
		Thread thread = new Thread(rproc);
		thread.start();
	    	long TIMEOUT = 120000; // 2 mins for now (sbml2matlab slow)  
		thread.join(TIMEOUT);
		if (thread.isAlive()) {
		    proc.destroy();
		    throw new Xcept(
		        progName + " timed out after " + TIMEOUT/1000
		        + " sec");
		}
		if (rproc.e != null) throw rproc.e;
		String errText = UtilIO.readText(proc.getErrorStream());
		int stat = rproc.stat;
		if (stat != 0) throw new Xcept(
		    progName + " error status=" + stat + ": " + errText);
		return UtilIO.readText(outFile);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// runnable process
	public static class ProcessRunnable implements Runnable {
	    public Process proc;
	    public int stat;
	    public Exception e;
	    public ProcessRunnable(Process proc) {
		this.proc = proc;
	    }
	    public void run() {
		try {
		    stat = proc.waitFor();
		} catch (InterruptedException e) {
		    this.e = e;
		}
	    }
	}

	// get warnings
	public StringList getWarnings() { return warnings; }
}

	    
