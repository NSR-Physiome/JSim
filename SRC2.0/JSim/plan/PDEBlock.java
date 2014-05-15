/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// PDE block of sequenced calculations

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.aserver.*;

import java.io.*;
import java.util.*;

public class PDEBlock extends MuBlock {
	protected Domain t; // time domain
	protected DomainSet xs; // space domains, if any
	protected String[] solverMsgs; // solver support foreach DETool
	protected Hashtable<TSubDom,BCBlock> bcblocks;
	protected PDEBlock[] splitBlocks; // null if unsplit

	// solver names for each dimensionality
	private static final String[] solverNames1D = new String[] {
	    "LSFEA", "MacCormack", "Toms731"
	};	
	
	// constructor
	public PDEBlock(SeqBlock parent, DETool detool,  
	Hashtable<VarUsage,Tool> vuTools) throws Xcept {
	    super(parent, vuTools);
	    t = detool.t();
	    xs = detool.xs;
	    muDoms.add(t);
	    muDoms.addAll(xs);
	    specDoms.addAll(muDoms);
	    title = "PDE Block-" + t + xs;
	}

	// add DETool
	protected void addDE(DETool detool) throws Xcept {
	    super.addDE(detool);
//	    DECon state = detool.state();
//	    for (int j=0; j<state.vreqs().size(); j++)
//		vsols.add(state.vreqs().get(j));
	}

	// build everything
	protected void build(boolean doSplits) throws Xcept {
	    log("Building " + this + " ...");
	    buildVsols();
	    setFactorVars();
	    buildSolvers();
	    buildMus();
	    buildOrder();
	    buildBCs();
	    if (doSplits) buildSplitBlocks();  
	}

	// build vsols,  exclude 1st spatial derivs of block PDEs
	private void buildVsols() throws Xcept {
	    for (int i=0; i<detools.size(); i++) {
		DETool detool = detools.get(i);
		DECon state = detool.state();
	    	for (int j=0; j<state.vreqs().size(); j++) {
		    VarUsage vu = state.vreqs().get(j);
		    if (box().isPDEFirstSpatialDeriv(vu)) {
		    	Var v0 = vu.v().zeroDeriv();
			if (vstate.contains(v0)) 
			    continue;
			else throw new AbortXcept(
	     		   "Variable " + vu + " unsolved in PDE block");
		    }		
		    vsols.add(vu);
	 	}
	    }
	}

	// check block vars for PDE factors
	private void setFactorVars() throws Xcept {
	    LinkedHashSet<Var> vblock = new LinkedHashSet<Var>(vstate);
	    for (int i=0; i<detools.size(); i++) {
	    	PDEFactors factors = factors(i);
		if (factors == null) continue;
		factors.setBlockVars(vblock);
		String msg = factors.factorMsg();
		if (msg == null) continue;
		throw new AbortXcept("Can't factor PDE " +
		    detools.get(i).v + ": " + msg);
	    }
	}

	// build solverMsgs
	private void buildSolvers() throws Xcept {
	    int n = (xs.size() == 1) ? 3 : 0;
	    solverMsgs = new String[n];
	    if (n == 0) return;
	    for (int id=0; id<n; id++) {
	        String s = buildSolverMsg(id);
		solverMsgs[id] = s;
		if (s == null) s = "supported";
		log("PDE solver " + solverName(id) + 
		    ": " + s);
	    } 
	}
	
	// support msg for one solver
	private String buildSolverMsg(int id) throws Xcept {
	    for (int j=0; j<detools.size(); j++) {
	    	String s = factors(j).solverMsg(id);
		if (s != null) return s;
	    }
	    return null;
	}		

	// build all BC blocks
	private void buildBCs() throws Xcept {
	    bcblocks = new Hashtable<TSubDom,BCBlock>();
	    Iterator<Domain> xiter = xs.iterator();
	    while (xiter.hasNext()) {
	    	Domain x = xiter.next();
		buildBC(model().lhbcs.get(x));
		buildBC(model().rhbcs.get(x));
	    }
	}
	
	// build 1 BC block
	private void buildBC(TSubDom bc) throws Xcept {
	    BCBlock block = new BCBlock(this, bc);
	    block.build(false);
	    bcblocks.put(bc, block);
	}

	// build split blocks
	protected void buildSplitBlocks() throws Xcept { 
	    if (! plan().splitBlocks()) return;
	    MuBlockSplitter splitter = new MuBlockSplitter(this);
	    int n = splitter.nSplitBlocks();
	    if (n < 2) return;
	    splitBlocks = new PDEBlock[n];
	    for (int i=0; i<n; i++) {
	    	splitBlocks[i] = (PDEBlock) splitter.getSplitBlock(i);
		splitBlocks[i].build(false);
	    }
	}

	// simple query
	public String toString() { return title() + " " + vstate; }
	public boolean matches(DETool detool) {
	    return t == detool.t() && xs.equals(detool.xs);
	}
	public Domain t() { return t; }
	public DomainSet xs() { return xs; }
	public int nx() { return xs.size(); };
	public int nsolvers() { 
	    return (nx() == 1) ? solverNames1D.length : 0; 
	}
	public String solverName(int id) { 
	    if (id < 0 || id >= nsolvers()) 
	        return "Unknown PDE solver id=" + id;
	    return solverNames1D[id];
 	}
	public String solverMsg(int id) { return solverMsgs[id]; }
	public PDEFactors factors(int i) { return detools.get(i).factors(); }
	public BCBlock bcblock(Domain x, boolean left) {
	    TSubDom sd = left ? 
	    	model().lhbcs.get(x) : model().rhbcs.get(x);
	    return bcblocks.get(sd);
	}
	public DomainSet loopDomains() throws Xcept {
	    DomainSet seqLoops = detools.get(0).seqLoops();
	    DomainSet loopDomains = seqLoops.minus(muDoms);
	    return loopDomains;
	}
	public PDEBlock[] splitBlocks() { return splitBlocks; }
}
