/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// var/tool cluster

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class MuCluster {
	private MuBlock block; // parent block
	private LinkedHashSet<Tool> tools; // tools in this block
	private LinkedHashSet<DECon> destates; // decons in this block
	private int ndes; // # detools
	private int nauxs; // # aux tools
	private double cbcost; // cost per callback
	private double cost; // total cost (cbcost * #callbacks)
	
	// constructor
	public MuCluster(MuBlock block) {
	    this.block = block;
	    tools = new LinkedHashSet<Tool>();
	    destates = new LinkedHashSet<DECon>();
	}

	// add a tool
	public void addTool(Tool tool) {
	    tools.add(tool);
	    if (tool instanceof DETool) {
	    	ndes++;
		destates.add(((DETool) tool).state());
	    } else {
	    	nauxs++;
	    }
	}

	// set costs
	public void setCosts(double cost, double cbcost) {
	    this.cost = cost;
	    this.cbcost = cbcost;
	}

	// query
	public int ndes() { return ndes; }
	public int nauxs() { return nauxs; }
	public int ntools() { return tools.size(); }
	public Iterator<Tool> toolIter() { return tools.iterator(); }
	public Iterator<DECon> destateIter() { return destates.iterator(); }
	public boolean isPDE() { return block instanceof PDEBlock; }
	public double cost() { return cost; }
	public double cbcost() { return cbcost; }
	public boolean hasTool(Tool tool) { return tools.contains(tool); }
	public boolean hasDEState(DECon con) { return destates.contains(con); }
}
