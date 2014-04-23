/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Connection (via server) to run-time model

package JSim.aserver;

import java.io.PrintStream;
import java.io.Serializable;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public interface ASModel {

	// job code and/or storeMode() values
	public static final int COMPILE = -1; // compile
	public static final int SINGLE = 0; // single run
	public static final int LOOPS = 1; // loops
	public static final int OPTIM = 2; // during optimization
	public static final int SENS = 3; // sensitivity analysis
	public static final int MOPT = 4;  // mult opt 

	// text types
	public static final int TEXT_XML = 0; // ambiguous XML file
	public static final int TEXT_MML = 1;
	public static final int TEXT_FLATMML = 2;
	public static final int TEXT_JAVA = 3;
	public static final int TEXT_SBML = 4;
	public static final int TEXT_CELLML = 5;
	public static final int TEXT_ANTIMONY = 6;
	public static final int TEXT_PLAN = 7;
	public static final int TEXT_MATHML = 9;
	public static final int TEXT_XMML = 10;
	public static final int TEXT_GRAPHML = 11;
	public static final int TEXT_MATLAB = 12;
	public static final String[] TEXT_NAMES = new String[] {
	    "XML", "MML", "Flat MML", "Java", "SBML", "CellML",
	    "Antimony", "Planner", "Units", "MathML", "XMML",
	    "GraphML" };

	// PDE solver IDs
	static public final int PDE_LSFEA = 0;
	static public final int PDE_MacCormack = 1;
	static public final int PDE_Toms731 = 2;

	static public String[] PDE_Solvers = new String[] {
	    "LSFEA", "MacCormack", "Toms731" };

	// JAVA MODEL CODE CONSTANTS
	static public final String JAVA_HDR = 
	    "// JSim generated model ";
	static public final String JSIM_MODEL_CLASS = 
	    "JSIM__MODEL__CLASS";

	// Memory Allocation Constants
	public static final int MEMORY_STATIC = 0;
	public static final int MEMORY_DYNAMIC = 1;
	public static final int MEMORY_SELECTED = 2;
	public static final int MEMORY_GRID_ALL = 0;
	public static final int MEMORY_GRID_NTH = 1;

	// unique model identifier within ASServer
  	public String modelID();

	// is model compiled (built)?
	public boolean isBuilt();

	// build the model
	public void buildRT(ASInfo.Build buildInfo) throws Xcept;

	// unbuild the model
	public void unbuildRT();

	// get build errors & warnings
	public String[] getBuildAlerts() throws Xcept;

	// retrieve variables (ASVars)
	public ASVar.List getASVars() throws Xcept;

	// retrieve variables (ASVars)
	public ASVar getASVar(String n) throws Xcept;

	// run model once with dynamic progress update
	public void singleRun(NamedVal.NList nvals) throws Xcept;

	// a loops run
	public void loopsRun(ASInfo.Loops loops) throws Xcept;
	
	// a sensitivity run
	public void sensRun(ASInfo.Sens sens) throws Xcept;
	
	// an optimization run
	public void optimRun(ASInfo.Optim optim) throws Xcept;
	
	// a multiple optimization run
	public void moptRun(ASInfo.Mopt mopt) throws Xcept;

	// get job info
	public ASInfo.JobInfo getJobInfo();
	
	// get latest job status (null if inapplicable or error)
	public ASInfo.Status getJobStatus();

	// skip to next loop
	public void nextLoop();

	// cancel run or build
	public void cancelJob();

	// parse a query
	public ASQuery parseQuery(String s) throws Xcept;

	// get data from stored run 
	public Data getData(int i, ASQuery query) throws Xcept;

	// get MOPT data
	public MoptData getMoptData() throws Xcept;

	// get name for run store
	public String getStoreName(int i);

	// internal model unit table
	public UnitNList units() throws Xcept;

	// get source or diagnostic text
	public String getText(int type, String variant) throws Xcept;

	// get warnings from getText() method
	public String[] getTextWarnings(int type, String variant) throws Xcept;
	
	// save mode, SINGLE if not compiled
	public int storeMode();

	// # runs stores
	public int nstores();

	// latest optimization results
	public OptimResults optimResults();

	// latest profile data
	public ProfileData getProfile() throws Xcept;

	// set func gen names
	public void setFuncGenNames(String[] names) throws Xcept;

	// get unit/solver flags
	public Flags getFlags();

	// static flag info
	public static class Flags implements Serializable {
	    public boolean needsUnits;
	    public boolean needsUnitCorrect;
	    public boolean usesODESolver;
	    public boolean[] usesPDESolvers;
	    public boolean usesFzero2Solver;
	    public boolean usesRandom;
	    public Flags() {
	    	usesPDESolvers = new boolean[PDE_Solvers.length];
	    }
	    public boolean usesPDESolver() {
		for (int i=0; i<usesPDESolvers.length; i++)
		    if (usesPDESolvers[i]) return true;
		return false;
	    }
	}
}

