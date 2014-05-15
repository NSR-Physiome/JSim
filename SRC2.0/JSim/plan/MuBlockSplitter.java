/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// split MuBlock into independent sub-blocks

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class MuBlockSplitter {
	private MuBlock block; // block to split
	private Hashtable<Var, DETool> vdetools; // v -> detool
	private LinkedHashMap<Var, LinkedHashSet<Var>> vgrps; // var groups
	private ArrayList<ArrayList<Var>> devgrps;
	private MuBlock[] splitBlocks; // generated sub-blocks
	private boolean valid; // split was successful

	// constructor
	public MuBlockSplitter(MuBlock block) throws Xcept {
	    this.block = block;
	    try {
	    	split();
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	}
	    
	// split the block
	private void split() throws Exception {
	    makeVdetools();
	    makeVgrps();
	    makeDeVgrps();
	}
	
	// create vdetools
	private void makeVdetools() throws Xcept {
	    vdetools = new Hashtable<Var, DETool>();
	    for (int i=0; i<block.detools().size(); i++) {
	    	DETool detool = block.detools().get(i);
		Var v = detool.vsols.get(0).v().zeroDeriv();
		vdetools.put(v, detool);
	    }
	}	    

	// make vgrps
	private void makeVgrps() throws Xcept {
	    // initial populate vgrps
	    vgrps = new LinkedHashMap<Var, LinkedHashSet<Var>>();
	    loadVgrps(block);
	    if (block instanceof PDEBlock) {
	    	PDEBlock pblock = (PDEBlock) block;
		Iterator<Domain> xiter = pblock.xs().iterator();
		while (xiter.hasNext()) {
		    Domain x = xiter.next();
		    loadVgrps(pblock.bcblock(x, true));
		    loadVgrps(pblock.bcblock(x, false));
		}
	    }
	    dump();

	    // augment vgrps until no change
	    boolean working = true;
	    while (working) {
	    	working = false;
		Iterator<LinkedHashSet<Var>> iter = 
		    vgrps.values().iterator();
		while (iter.hasNext()) {
		    LinkedHashSet<Var> vgrp = iter.next();
		    ArrayList<Var> varr = new ArrayList<Var>(vgrp);
		    for (int i=0; i<varr.size(); i++) {
		    	Var v1 = varr.get(i);
		    	LinkedHashSet<Var> vgrp1 = vgrps.get(v1);
			boolean add = vgrp.addAll(vgrp1);
			if (add) working = true;
		    }
		}
		if (working) {
//		    log("\nLooping vgrps ...");
//	            dump();
		}
	    }
	}

	// load vgrps from single MuBlock
	private void loadVgrps(MuBlock block) throws Xcept {
	    for (int i=0; i<block.detools.size(); i++) {
	    	DETool detool = block.detools.get(i);
		Var v = detool.v;
	        VarUsages vus = detool.state().vreqs();
		loadVgrps(v, vus);
	    }
		
	    for (int i=0; i<block.items().size(); i++) {
	    	SeqItem item = block.items().get(i);
		if (! (item instanceof Tool)) continue;
		Tool tool = (Tool) item;
//		log("loading " + tool);
		for (int j=0; j<tool.vsols.size(); j++) {
		    Var v = tool.vsols.get(j).v().zeroDeriv();
		    loadVgrps(v, tool.vreqs);
		}
	    }
	}
	
	// load vgrps for v & vreqs
	private void loadVgrps(Var v, VarUsages vreqs) throws Xcept {
	    if (v instanceof Domain) return;
	    LinkedHashSet<Var> grp = vgrps.get(v);
	    if (grp == null) {
		grp = new LinkedHashSet<Var>();
		grp.add(v);
		vgrps.put(v, grp);
	    }
	    for (int k=0; k<vreqs.size(); k++) {
	    	Var v1 = vreqs.get(k).v().zeroDeriv();
		if (v == v1) continue;
		if (v1 instanceof Domain) continue;
		if (! block.vmus.hasVar(v1)) continue;
		grp.add(v1);
	    }
	}

	// create degrps
	private void makeDeVgrps() { 
	    LinkedHashSet<Var> vstate = new LinkedHashSet<Var>(block.vstate());
	    devgrps = new ArrayList<ArrayList<Var>>();
	    while (! vstate.isEmpty()) {
	    	Iterator<Var> iter = vstate.iterator();
		Var v = iter.next();
		LinkedHashSet<Var> vgrp = vgrps.get(v);
		log("var " + v + " -> grp " + vgrp);
		if (vgrp == null) {
		    vstate.remove(v);
		    continue;
		}
		ArrayList<Var> vs = new ArrayList<Var>();
		devgrps.add(vs);
		Iterator<Var> iter1 = vgrp.iterator();
		while (iter1.hasNext()) {
		    Var v1 = iter1.next();
		    if (vdetools.get(v1) == null) continue;
		    vs.add(v1);
		    vstate.remove(v1);
		}
		log("\t" + vs);
	    }
	    log("#blocks=" + devgrps.size());	
	}

	
	// query split blocks
	protected int nSplitBlocks() { 
	    return (devgrps == null) ? 0 : devgrps.size();
	}
	protected MuBlock getSplitBlock(int b) throws Xcept { 
	    ArrayList<Var> devgrp = devgrps.get(b);
	    Var v = devgrp.get(0);
	    DETool detool = vdetools.get(v);
	    MuBlock splitBlock = (block instanceof ODEBlock) ? 
	    	new ODEBlock(block, detool, block.vuTools) :
	    	new PDEBlock(block, detool, block.vuTools);
	    for (int i=0; i<devgrp.size(); i++) {
	    	v = devgrp.get(i);
	    	detool = vdetools.get(v);
	    	splitBlock.addDE(detool);
	    }
	    log("getSplitBlock: " + splitBlock);
	    return splitBlock;
	}	    

	// dump 
	private void dump() {
	    Iterator<Var> vs = vgrps.keySet().iterator();
	    while (vs.hasNext()) {
	    	Var v = vs.next();
		LinkedHashSet<Var> vgrp = vgrps.get(v);
		log("" + v + "\t" + vgrp);
	    }
	}

	// log message
	private void log(String s) {
	    Util.verbose(s);
	    // System.err.println(s);
	}
}

 
