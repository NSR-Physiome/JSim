/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// copy of tool reused in sequencing, restricted to IC 

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ReuseTool extends Tool {
	public Tool tool; // reuse of this tool
	public Domain x; // IC domain (not t, which implies loop)
	public TSubDom lhbc; // of x

	// constructor
	public ReuseTool(Tool tool, Domain x) throws Xcept {
	    super(tool.model);
	    this.tool = tool;
	    this.x = x;
	    lhbc = model.lhbcs.get(x);
	    vsols = tool.vsols.restrict(lhbc);
	    vreqs = tool.vreqs.restrict(lhbc);
	    seqLoops = new DomainSet(tool.seqLoops);
	    if (! tool.seqLoops.contains(x)) throw new Xcept(
	    	"ReuseTool requires seqLoops containing " + x + 
		": " + this);
	    seqLoops.remove(x);
	}

	// set seq loops
	protected void setSeqLoops() { 
	    // done in constructor
	}

	// simple query
	public String toString() { 
	    return "when (" + lhbc + ") " + tool; 
	}
	public String toolType() { return tool.toolType(); }
}
