/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// v2.0 planner

package JSim.lserver;

import java.io.*;
import java.lang.reflect.*;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 

import JSim.util.*;
import JSim.mml.*;
import JSim.plan.*;
import JSim.jcode.*;
import JSim.aserver.*;
import JSim.mathml.*;
import JSim.data.*;

public class LSPlan implements Model.Translator {
	private LSServer server; // for this server
	private String mmlSource; // original (unflattened) source
	private File flatFile; // if flattening reqd
	private NamedVal.NList options; // planner options

	private Model flatModel; // flattened model
	private Plan plan;	

	// constructor
	public LSPlan(LSServer server, String mmlSource,
	File flatFile,  NamedVal.NList options) 
	throws Xcept {
	    this.server = server;
	    this.mmlSource = mmlSource;
	    this.flatFile = flatFile;
	    this.options = options;
	}

	// make plan
	public void makePlan() throws Xcept {

	    // parse model source
	    // add \n to modelSource for final-line // comments
	    StringReader rdr = new StringReader(mmlSource + "\n");
	    flatModel = new ModelReader(rdr, 
		server.classLoader, server.jsimPath());
	    flatModel.setTranslator(this);
		
	    // flatten model 
	    flatModel.flatten(flatFile.getPath()); 
	    MathSys math = flatModel.getFlatMath();
		
	    // planning
	    plan = new Plan(flatModel);
	    plan.setOptions(options);
	    if (Util.verbose) plan.setVerbose("");
	    plan.process();
	    StringList errors = plan.logger.errors;
	    if (errors.size() > 0) 
	        throw new Xcept(errors.get(0));
	}

	// Model.Translator: translate antimony 2 MML
	public String antimony2MML(String text) throws Xcept {
	    return server.translateModelText(ASModel.TEXT_ANTIMONY,
	        ASModel.TEXT_MML, text, null);
	}

	// get build errors & warnings
	public String[] getBuildAlerts() {
	    if (plan == null) return null;
	    return plan.logger.alerts.array();
	}
	
	// write Java
	public void writeJava(String outbase, File javaFile)
	throws Xcept {
	    JPlanWriter wrt = new JPlanWriter(outbase, plan);
	    wrt.setFile(javaFile);
	    wrt.writePlan();
	}

	// plan text
	public String getPlanText(String variant) throws Xcept {
	    if (plan == null) return null;
	    return plan.getPlanText();
	}

	// XMML text
	public String getXMMLText() throws Xcept {
	    try {
	    	XMMLWriter xmml = new XMMLWriter(plan);
	    	return xmml.writeString();
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// GraphML text
	public String getGraphMLText() throws Xcept {
	    XMLWriter wrt = new XMLWriter();
	    return wrt.writeString(plan.getGraphML());
	}

	// add model/var properties
	public void addProperties(NamedVal.NList props) throws Xcept {
	}

	// get flat model
	public Model getFlatModel() { return flatModel; }
}
