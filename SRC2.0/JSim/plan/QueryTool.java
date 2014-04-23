/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// var solved via query of other another solved var

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class QueryTool extends Tool {
	protected VarUsage vu;

	// constructor for var query
	public QueryTool(VarUsage vu) throws Xcept {
	    super(vu.model());
	    vsols.add(vu);
	    vreqs.add(vu.v());
	    this.vu = vu;
	    setSeqLoops();
	}

	// constructor for non-var vreq
	public QueryTool(VarUsage vu, VarUsage vreq) throws Xcept {
	    this(vu);
	    vreqs = new VarUsages(model);
	    vreqs.add(vreq);
	}
	    
	// set seq loops
	protected void setSeqLoops() throws Xcept { 
	    seqLoops = vu.seqLoops();
//	    if (! vu.v().isDomain())
//	    	seqLoops = safeSeqLoops();
//System.err.println("seqLoops=" + seqLoops + " for " + this);
	}

	// safe seqloops, calc xexprs external to loop, min internal
	// NOT USED: this seems to cause more problems than it solves
	protected DomainSet safeSeqLoops() throws Xcept {
	    DomainSet xs = new DomainSet();
	    Var v = vu.v();
	    for (int i=0; i<v.ndim(); i++) {
		Domain x = v.domain(i);
	    	switch (vu.qstat(x)) {
		case MIN:
		case CURR:
		case DELAY:
		    xs.add(x);
		}
	    }
	    return xs;
	}
	
	// delay seqloops, calc xexprs & min within loop
	protected DomainSet delaySeqLoops() throws Xcept {
	    DomainSet xs = new DomainSet();
	    Var v = vu.v();
	    for (int i=0; i<v.ndim(); i++) {
		Domain x = v.domain(i);
	    	switch (vu.qstat(x)) {
		case MIN:
		case CURR:
		case DELAY:
		case XEXPR:
		    xs.add(x);
		}
	    }
	    return xs;
	}

	// query contains any xexprs?
	protected boolean hasXexpr() throws Xcept {
	    Var v = vu.v();
	    for (int i=0; i<v.ndim(); i++) {
	    	Domain x = v.domain(i);
		if (vu.qstat(x) == XEXPR) return true;
	    }
	    return false;
	}

	// simple query
	public String toString() { 
	    return "query " + vsols; 
	}
	public String toolType() { return "query"; }
}
