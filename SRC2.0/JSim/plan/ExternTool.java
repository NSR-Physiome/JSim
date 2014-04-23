/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// solves extern variable

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ExternTool extends Tool {
	public Var v;   // variable solved
	
	// constructor
	public ExternTool(TModel model, Var v) throws Xcept {
	    super(model);
	    this.v = v;
	    vsols.add(v);
	}

	// query
	public Var v() { return v(); }
	public String toString() { return "extern " + v; }
	public String toolType() { return "extern"; }
}
