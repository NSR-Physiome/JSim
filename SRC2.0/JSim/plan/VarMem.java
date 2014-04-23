/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Tell run-time that variable is no longer needed for model internal calculations
// If memory is limited, the RT may elect to free memory storage at this point
//   without impairing model calculations
// For efficiency, VarMem items are not created for 0d vars

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class VarMem implements SeqItem {
	private boolean isFree; 
	private ArrayList<Var> vars;

	// constructor
	public VarMem(boolean isFree) {
	    this.isFree = isFree;
	    vars = new ArrayList<Var>();
	}
	public VarMem(boolean isFree, Var v) {
	    this(isFree);
	    add(v);
	}
	public VarMem(boolean isFree, ArrayList<Var> vars) {
	    this(isFree);
	    addAll(vars);
	}
	
	// add vars
	public void add(Var v) { vars.add(v); }
	public void addAll(Collection<Var> vs) { vars.addAll(vs); }

	// public query
	public boolean isFree() { return isFree; }
	public boolean isAlloc() { return !isFree; }
	public String toString() {  
	    return (isFree ? "free " : "alloc ") + vars; 
	}
	public int size() { return vars.size(); }
	public ArrayList<Var> vars() { return vars; }
	public Domain t() { return null; }
	public VarUsages vreqs() { return null; }
	public DomainSet seqLoops() { return null; }
	public String nodeString() { return toString(); }
	
}


