/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim model plan: (1st pass of model compiler)

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;
import org.w3c.dom.*;

public class Plan {
	public Logger logger; // msg handling
	private Model flatModel; // flattened MML model
	private NamedVal.NList options; // planner options
	private TModel model; // plan-time model
	private ToolBox box; // tool box
	private MainBlock main; // main sequence

	// constructor
	public Plan(Model model) {
	    logger = new Logger();
	    flatModel = model;
	    options = new NamedVal.NList();
	}

	// set logging
	public void setVerbose(String vtags) {
	    logger.setVerbose(vtags);
	}

	// set options
	public void setOptions(NamedVal.NList options) {
	    this.options = options;
	    log("Plan options=" + options);
	    logger.setTimeout(timeout());
	    logger.setSaveGraphs(saveUntanglerGraphs());
	}
	
	// process the model
	public void process() throws Xcept {

	    // model init phase
	    try {
	    	model = new TModel(this, flatModel);
	    } catch (Exception e) {
	    	logger.log(e);
  	    }
	    logPhase("Input     ");
	    if (logger.nerrors() > 0) return;

	    // toolbox phase
	    box = new ToolBox(this);
	    try {
	    	box.build();
	    } catch (Exception e) {
	    	logger.log(e);
  	    }
	    logPhase("Toolbox   ");
	    if (logger.nerrors() > 0) return;
	    if (! makeSequence()) return;

	    // sequencing phase
	    main = new MainBlock(box);
	    try {
	    	main.build();
	    } catch (Exception e) {
	    	logger.log(e);
  	    }
	    logPhase("Sequencing");
	}

	// log
	public void log(String msg) { logger.log(msg); }
	public void log(int code, String msg) { logger.log(code, msg); }
	public void logPhase(String phase) {
	    String s = (logger.nerrors() > 0) ? 
	        "aborted  " : "completed";
	    log(phase + " phase " + s + " with " + 
	        logger.nerrors() + " errors, " + 
		logger.nwarnings() +  " warnings.");
	}

	// structure query
	public TModel model() { return model; }
	public ToolBox box() { return box; }
	public MainBlock main() { return main; } 

	// options query
	public int timeout() {
	    return options.intVal("timeout", 5);
	}
	public int maxImplicitBlock() {
	    int i = options.intVal("maxImplicitBlock", 10);
	    if (i < 10) i = 10; // was 3 in most v1.6 projects
	    return i;
	}
	public double maxImplicitSearch() {
	    return options.realVal("maxImplicitSearch", 1e6);
	}
	public boolean allowMissingDomainControls() {
	    return options.boolVal("allowMissingDomainControls", true);
	}
	public boolean allowMissingICs() {
	    return options.boolVal("allowMissingICs", false);
	}
	public boolean allowOrphanICs() {
	    return options.boolVal("allowOrphanICs", false);
	}
	public boolean ignoreUnusedEquations() {
	    return options.boolVal("ignoreUnusedEquations", false);
	}
	public boolean makeDEICParms() {
	    return options.boolVal("makeDEICParms", true);
	}
	public boolean makeStateICParms() {
	    return options.boolVal("makeStateICParms", false);
	}
	public boolean makeSequence() {
	    return options.boolVal("makeSequence", true);
	}
	public int seqMaxPulls() {
	    return options.intVal("seqMaxPulls", 10);
	}
	public boolean makeDerivTools() {
	    return options.boolVal("makeDerivTools", true);
	}
	public boolean makeInputTools() {
	    return options.boolVal("makeInputTools", true);
	}
	public boolean initNaN() {
	    return options.boolVal("initNaN", true);
	}
	public boolean abortNaN() {
	    return options.boolVal("abortNaN", false);
	}
	public boolean traceNaN() {
	    return options.boolVal("traceNaN", false);
	}
	public String traceVars() {
	    return options.stringVal("traceVars", "");
	}
	public int untanglerVersion() {
	    return options.intVal("u", 3);
	}
	public boolean saveUntanglerGraphs() {
	    return options.boolVal("usave", true); // false for release
	}
	public boolean pullDisjoint() {
	    return options.boolVal("pullDisjoint", true);
	}
	public boolean splitBlocks() {
	    return options.boolVal("splitBlocks", true);
	}
	public boolean parallelize() {
	    return options.boolVal("parallelize", false);
	}

	// get GraphML
	public Document getGraphML() throws Xcept {
	    PlanGraphML gml = new PlanGraphML(this);
	    return gml.getXML();
	}

	// get Plan text
	public String getPlanText() throws Xcept {
	    if (main == null) return null;
	    StringWriter swrt = new StringWriter();
	    PrintWriter pwrt = new PrintWriter(swrt);
	    pwrt.println("==== Planner Log ====");
	    for (int i=0; i<logger.msgs.size(); i++) 
	        pwrt.println(logger.msgs.get(i));
	    if (main != null) {
		pwrt.println("");
		pwrt.println("==== Main Calculation Block ====");
		main.dump(pwrt);
	    }
	    return swrt.toString();
	}
}
