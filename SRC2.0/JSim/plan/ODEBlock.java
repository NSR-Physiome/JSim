/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ODE block of sequenced calculations

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class ODEBlock extends MuBlock {
	private Domain t; // time domain
	protected ODEBlock[] splitBlocks; // null if unsplit
	
	// constructor
	public ODEBlock(SeqBlock parent, DETool detool,  
	Hashtable<VarUsage,Tool> vuTools) throws Xcept {
	    super(parent, vuTools);
	    t = detool.t();
	    muDoms.add(t);
	    specDoms.addAll(muDoms);
	    title = "ODE Block-" + t;
	}

	// add DETool
	protected void addDE(DETool detool) throws Xcept {
	    super.addDE(detool);
	    DECon state = detool.state();
	    for (int j=0; j<state.vreqs().size(); j++)
		vsols.add(state.vreqs().get(j));
	    Tool stool = state.tool(); // ODE always 
	    for (int i=0; i<stool.vsols.size(); i++) {
	    	VarUsage vu = stool.vsols.get(i);
		if (! vu.isCurr()) continue;
		vsols.add(vu);
	    }
	}

	// build split blocks
	protected void buildSplitBlocks() throws Xcept { 
	    if (! plan().splitBlocks()) return;
 	    MuBlockSplitter splitter = new MuBlockSplitter(this);
	    int n = splitter.nSplitBlocks();
	    if (n < 2) return;
	    splitBlocks = new ODEBlock[n];
	    for (int i=0; i<n; i++) {
	    	splitBlocks[i] = (ODEBlock) splitter.getSplitBlock(i);
		splitBlocks[i].build(false);
	    }
	}

	// simple query
	public String toString() { return title + " " + vstate; }
	public boolean matches(DETool detool) {
	    return t == detool.t() && detool.xs.size() == 0;
	}
	public Domain t() { return t; }
	public ODEBlock[] splitBlocks() { return splitBlocks; }
}
