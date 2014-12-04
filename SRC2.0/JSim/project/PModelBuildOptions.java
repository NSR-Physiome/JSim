/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Model compilation options section of a Project

package JSim.project; import JSim.aserver.*;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class PModelBuildOptions extends PNamed {

	// controls
	public IntControl maxBuildTime;
	public BooleanControl initNaN;
	public BooleanControl abortNaN;
	public BooleanControl traceNaN;
	public StringControl traceVars;
	public IntControl maxImplicitBlock;
	public BooleanControl ignoreAllOverspec;
	public BooleanControl ignoreAllUnderspec;
	public BooleanControl splitBlocks;
	public BooleanControl parallelize;
	
	// constructor
	public PModelBuildOptions(PModel p, String n) throws Xcept {
	    super(p, n);
	    maxBuildTime = new IntControl(
	    	this, "maxBuildTime", 120);
	    maxImplicitBlock = new IntControl(
	    	this, "maxImplicitBlock", 10);
	    initNaN = new BooleanControl(this, "initNaN", false);
	    abortNaN = new BooleanControl(this, "abortNaN", false);
	    traceNaN = new BooleanControl(this, "traceNaN", false);
	    traceVars = new StringControl(this, "traceVars");
	    ignoreAllOverspec = new BooleanControl(
	    	this, "ignoreAllOverspec", false);
	    ignoreAllUnderspec = new BooleanControl(
	    	this, "ignoreAllUnderspec", false);
	    splitBlocks = new BooleanControl(
	    	this, "splitBlocks", false);
	    parallelize = new BooleanControl(
	    	this, "parallelize", false);
	}

	// query
	public String xmlLabel() { return "options"; }
	public String diagInfo() { 
	   return parent().name() + " build options";
	}

}

