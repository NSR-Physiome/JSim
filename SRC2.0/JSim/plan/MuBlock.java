/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// microstepped block of sequenced calculations

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

abstract public class MuBlock extends SeqBlock {
	protected String title; // for printing
	protected Hashtable<VarUsage,Tool> vuTools; // tool solving vus
	protected ArrayList<DETool> detools; // state tools
	protected ArrayList<Var> vstate; // state vars
	protected DomainSet muDoms; // muStepped domains
	protected VarUsages vmus; // mustep vus
	protected VarUsages vsols; // vars to solve

	// processing
	private Hashtable<Tool, Boolean> muToolMap;
	private HashSet<Tool> muToolSet;
	private VarUsages muSolved; // vus solved by muToolMap
	private VarUsages orderSolved; // vus so far solved in block
	
	// constructor
	public MuBlock(SeqBlock parent, Hashtable<VarUsage,Tool> vuTools) throws Xcept {
	    super(parent);
	    this.vuTools = vuTools;
	    title = "untitled block";
	    detools = new ArrayList<DETool>();
	    vstate = new ArrayList<Var>();
	    muDoms = new DomainSet();
	    vmus = new VarUsages(model());
	    vsols = new VarUsages(model());
	}

	// add DE to this
	protected void addDE(DETool detool) throws Xcept {
	    detools.add(detool);
	    for (int i=0; i<detool.vsols.size(); i++) {
	    	VarUsage vu = detool.vsols.get(i);
		if (! vu.isCurr()) continue;
		vstate.add(vu.v());
	    }
	}
	
	// build sequence
	protected void build(boolean doSplits) throws Xcept {
	    log("Building " + this + " ...");
	    buildMus();
	    buildOrder();
	    if (doSplits) 
	    	buildSplitBlocks();  
	}

	// build block splitter
	protected void buildSplitBlocks() throws Xcept { 
	    // implemented in ODE & PDE blocks only	
	}

	// build vmus and muToolMap for this block
	protected void buildMus() throws Xcept {
	    muToolMap = new Hashtable<Tool, Boolean>();
	    muToolSet = new LinkedHashSet<Tool>();
	    vmus.add(muDoms);
	    vmus.add(vstate);
//log("vmus start: " + vmus + " vsols=" + vsols);
	    for (int i=0; i<vsols.size(); i++) {
//log("--buildMu " + vsols.get(i));
	    	buildMu(vsols.get(i));
	    }
	}

	// check if muVar, if so add var & tool
	protected void buildMu(VarUsage vu) throws Xcept {
//log("  check " + vu + " has=" + vmus.contains(vu));
	    if (vmus.contains(vu)) return;
	    Tool tool = vuTools.get(vu);
//log("     " + vu + " tool=" + tool + " buildMu(tool)=" +buildMu(tool));
	    if (! buildMu(tool)) return;
	    log("  adding muVar " + vu);
	    vmus.add(vu);
	    for (int i=0; i<tool.vreqs().size(); i++) 
	    	buildMu(tool.vreqs().get(i));
	}

 	// is tool muStepped? if new result, save in muToolMap
	private boolean buildMu(Tool tool) throws Xcept {
	    Boolean b = muToolMap.get(tool);
	    if (b != null) return b.booleanValue();
	    boolean ret;
	    if (! tool.seqLoops().containsAny(muDoms)) 
	    	ret = false;
	    else if (tool instanceof DETool)
	    	ret = false;
	    else if (! specDoms.containsAll(tool.seqLoops())) {
	    	ret = false; // in future, create subMu loopBlock?
//System.err.println("Now rejecting " + tool + " (loops="+ 
//tool.seqLoops() + ") from " + this + " specDoms=" + specDoms);
	    } else if (box().isInput(tool))
	        ret = true;
	    else if (tool instanceof ExprTool)
	        ret = buildMu(tool.vreqs());
	    else if (tool instanceof ImplicitTool)
	        ret = buildMu(tool.vreqs());
	    else if (tool instanceof QueryTool)
	    	ret = buildMu(tool.vreqs());
	    else if (tool instanceof DomainTool)
	    	ret = false;
	    else if (tool instanceof StateTool)
	    	ret = false;
	    else if (tool instanceof ExternTool)
	    	ret = true;  // change if live-feed extern???
	    else if (tool instanceof ProcTool)
	        ret = false; // needs run-time allocation ???
	    else if (tool instanceof ReuseTool)
	        ret = false;
	    else
	        throw new Xcept("MuBlock: unrecognized tool " + tool);
	    b = ret ? Boolean.TRUE : Boolean.FALSE;
//log("  toolMap " + tool + " == " + b);
	    muToolMap.put(tool, b);
	    if (ret) {
	        log("  adding muTool " + tool);
		muToolSet.add(tool);
	    } 
	    return ret;
	}	    	

	// can any vu in this set can be muStepped?
	private boolean buildMu(VarUsages vus) throws Xcept {
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (vmus.contains(vu)) return true;
		Tool tool = vuTools.get(vu);
	    	if (buildMu(tool)) return true;
	    }
	    return false;
	}

	// add muToolMap to block in correct calculation order
	protected void buildOrder() throws Xcept {
	    muSolved = new VarUsages(model());	
	    Iterator<Tool> tools = muToolSet.iterator();
	    while (tools.hasNext()) {
	    	Tool tool = tools.next();
		muSolved.add(tool.vsols);
	    }
	    orderSolved = new VarUsages(model());
	    while (buildOrderPass());
	    if (size() != muToolSet.size()) throw new Xcept(
	    	"MuBlock: internal sequencing error");
	}
	
	// add muToolMap that are ready to be ordered, ret if added
	private boolean buildOrderPass() throws Xcept {
	    boolean working = false;
	    Iterator<Tool> tools = muToolSet.iterator();
	    while (tools.hasNext()) {
	    	Tool tool = tools.next();
		if (items().contains(tool)) continue;
		if (isReady(tool)) {
		    add(tool);
		    orderSolved.add(tool.vsols);
		    working = true;
		}
	    }
	    return working;		     
	}

	// is tool ready to be added to order?
	private boolean isReady(Tool tool) throws Xcept {
	    for (int i=0; i<tool.vreqs.size(); i++) {
	    	VarUsage vu = tool.vreqs.get(i);
		if (! muSolved.contains(vu)) continue;
		if (orderSolved.contains(vu)) continue;
		return false;
	    }
	    return true;
	}	

	// simple query
	public String title() { return title; }
	public boolean matches(DETool tool) { return false; }
	public ToolBox box() { return model().plan.box(); }
	public ArrayList<Var> vstate() { return vstate; }
	public DomainSet muDoms() { return muDoms; } 
	public VarUsages vmus() { return vmus; } 
	public ArrayList<DETool> detools() { return detools; }

	// message
	public void log(String msg) { model().log(msg); }
}
