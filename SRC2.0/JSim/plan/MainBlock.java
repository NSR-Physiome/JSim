/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// main sequencing phase of compiler

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class MainBlock extends SeqBlock {
	public ToolBox box;
	public Plan plan;
	public TModel model;
	protected ArrayList<Tool> tools; // tools to sequence
	protected ArrayList<TEvent> events;
	protected ArrayList<TRelation> relations;
	private SeqMem seqMem;  // memory optimization

	// work fields
	protected Hashtable<VarUsage, Tool> vuTools;
	protected ArrayList<ImplicitBound> implicitBounds;
	protected LinkedHashSet<SeqItem> retryItems; // nonstd sequencing
	private MainGraph igraph;  // initial sequencing graph
	private Hashtable<SeqNode, SeqItem> nodeItems; // tools 4 nodes
	private SeqGraphCirc cgraph;   // circular deps (type 1)
	private SeqGraph ugraph;  // untangled graph
	private SeqGraph wgraph;  // block-building graph
	private LinkedHashSet<SeqPhase> wphases; // phases blocked so far
  	private Hashtable<SeqPhase,LinkedHashSet<SeqNode>> phaseFFTNodes;
	private ArrayList<MuBlock> deblocks;

	// constructor
	public MainBlock(ToolBox box) throws Xcept {
	    super(box.plan.model());
	    this.box = box;
	    plan = box.plan;
	    model = plan.model();
	    tools = new ArrayList<Tool>(box.seqTools());
	    events = model.events;
	    relations = model.relations;
	}

	// build main sequence
	protected void build() throws Xcept {
	    log("\nSequencing phase starting ...");
	    for (int i=0; i<tools.size(); i++) 
	        tools.get(i).setSeqLoops();
	    if (logger().nerrors()>0) return;
	    logItems();
	    log("Creating vuTools map ...");
	    buildVuTools();
	    buildImplicitBounds();
	    checkEventToolCompatibility();
	    retryItems = new LinkedHashSet<SeqItem>();
	    while (! createGraph()); // create graph: loop retryItems
	    mergeImplicitBounds();
	    removeImplicitBoundNodes(ugraph);
	    log("Building mainline block ...");
	    deblocks = new ArrayList<MuBlock>();
	    wgraph = new SeqGraph(ugraph);
	    log("Mainline block graph:");
	    // dump(wgraph);
	    wphases = new LinkedHashSet<SeqPhase>();
  	    phaseFFTNodes = new Hashtable<SeqPhase,LinkedHashSet<SeqNode>>();
	    buildPhaseBlock(this, wgraph.mainPhase());
	    for (int i=0; i<deblocks.size(); i++) {
	    	MuBlock block = deblocks.get(i);
		block.build(true);
	    }

	    // Optimize run-time memory usage (in development)
	    try {
//	    	seqMem = new SeqMem(this);
	    	seqMem.process();
 	    } catch (Exception e) {
	    	// logger().log(e); // still not completely working
	    }

	    if (logger().isVerbose("b") && logger().out != null) 
	    	dump(logger().out);
	}

	// build vuTools
	private void buildVuTools() throws Xcept {
	    vuTools = new Hashtable<VarUsage, Tool>();
	    for (int i=0; i<tools.size(); i++) 
		updateVuTools(tools.get(i));
	    log("Adding reuse & query tools ...");
	    while (buildReuseTools());
	    buildQueryTools();
	}

	// add reuse tools, return if still working
	private boolean buildReuseTools() throws Xcept {
	    boolean working = false;
	    ArrayList<VarUsage> vus = new ArrayList<VarUsage>(model.vus);
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vmu = vus.get(i);
		if (vuTools.get(vmu) != null) continue;
		if (isUnsolvedPDEDeriv(vmu)) continue;
		if (vmu.stat() != VarUsage.MIN) continue;
		VarUsage vu = new VarUsage(model, vmu.v());
		Tool vtool = vuTools.get(vu);
		if (vtool == null) throw new Xcept(
		     "MainBlock: No tool for " + vu);
		Domain x = vmu.domain();
		if (! vtool.seqLoops().contains(x)) continue;
		if (vtool instanceof DETool) continue;
		Tool rtool = new ReuseTool(vtool, x);
		tools.add(rtool);
		log("  adding reuse tool: " + rtool
		    + " " + rtool.vsols + "<<" + rtool.vreqs);
		updateVuTools(rtool);
		working = true;
	    }
	    return working;
	}

	// add query tools
	private void buildQueryTools() throws Xcept {
	    Iterator<VarUsage> vus = model.vus.iterator();
	    while (vus.hasNext()) {
	    	VarUsage vu = vus.next();
		if (vuTools.get(vu) != null) continue;
		if (isUnsolvedPDEDeriv(vu)) continue;
		if (vu.isCurr()) 
		    throw new Xcept(
		        "MainBlock: No tool for " + vu);

		if (vu.isBoundary() && ! vu.isSolvable()) {
		    VarUsage vu0 = new VarUsage(model, vu.v(),
		        vu.stat(), vu.domain());
		    Tool tool = vuTools.get(vu0);
		    if (tool != null) {
			System.err.println("ignoring query tool for " + vu);
			vuTools.put(vu, tool);
		        continue;
		    }
		}

		Tool qtool = makeQueryTool(vu);
		log("  adding query tool: " + qtool + "<<" + qtool.vreqs());
		tools.add(qtool);
		updateVuTools(qtool);
	    }
	}

	// create a query tool
	private QueryTool makeQueryTool(VarUsage vu) throws Xcept {
	    // return new QueryTool(vu);
	    Var v = vu.v();
	    if (box.mainTools.get(v) instanceof DETool) {
	    	DETool detool = (DETool) box.mainTools.get(v);
		Domain t = detool.t();
		if (vu.qstat(t) == VarUsage.MIN) {
		    VarUsage vreq = new VarUsage(model, v, 
	        	VarUsage.MIN, t);
		    return new QueryTool(vu, vreq);
		}
	    }
	    return new QueryTool(vu);
	}	    


	// update vuTools and vuMins
	private void updateVuTools(Tool tool) throws Xcept {
	    for (int j=0; j<tool.vsols.size(); j++) {
		VarUsage vu = tool.vsols.get(j);
		vuTools.put(vu, tool);
	    }
	}	    

	// if vu not solved because exists only for PDE spec?
	private boolean isUnsolvedPDEDeriv(VarUsage vu) 
	throws Xcept {
	    Var v = vu.v();
	    if (! v.isDeriv()) return false;
	    VarUsage v0u = new VarUsage(model, v.zeroDeriv());
	    Tool tool = vuTools.get(v0u);
	    if (! (tool instanceof DETool)) return false;
	    DETool detool = (DETool) tool;
	    return detool.xs.size() > 0;
	}

	// check event/tool compatibility
	private void checkEventToolCompatibility() throws Xcept {
	    for (int i=0; i<events.size(); i++) {
	    	TEvent event = events.get(i);
		for (int j=0; j<event.vacts.size(); j++) {
		    VarUsage vu = event.vacts.get(j);
		    if (! vu.isCurr()) continue;
		    Tool tool = vuTools.get(vu);
		    if ((tool == null)
		    || (tool instanceof StateTool)
		    || (tool instanceof DETool))
		       continue;
		    throw new AbortXcept("" + vu + 
		    	": Events incompatible with " + tool);
		}
	    }
	}

	// create implicit bounds
	private void buildImplicitBounds() throws Xcept {
	    // collect implicit vus
	    VarUsages implicitVus = new VarUsages(model);
	    ArrayList<VarUsage> vus = new ArrayList<VarUsage>(model.vus);
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (vuTools.get(vu) instanceof ImplicitTool)
		    implicitVus.add(vu);
	    }
 
	    // create relationBounds
	    implicitBounds = new ArrayList<ImplicitBound>();
	    for (int i=0; i<relations.size(); i++) {
	    	TRelation r = relations.get(i);
		ImplicitBound bound = ImplicitBound.create(r, implicitVus);
		if (bound == null) continue;
		Tool tool = vuTools.get(bound.vu);
		if (! tool.seqLoops().equals(r.seqLoops()))
		    continue;
		if (! bound.setTool((ImplicitTool) tool))
		    continue;
		implicitBounds.add(bound);
		logger().log("i", 
		    "Implicit Bound: " + bound + " from " + r);
	    }
	}

	// merge ImplicitBounds with ImplicitTools
	private void mergeImplicitBounds() throws Xcept {
	    for (int i=0; i<implicitBounds.size(); i++) {
	    	ImplicitBound b = implicitBounds.get(i);
		if (retryItems.contains(b)) continue;
		b.tool().addBound(b);
	    }
	}

	// remove ImplicitBound nodes from graph
	private void removeImplicitBoundNodes(SeqGraph graph) 
	throws Xcept {
	    for (int i=0; i<implicitBounds.size(); i++) {
	    	ImplicitBound b = implicitBounds.get(i);
		if (retryItems.contains(b)) continue;
	        String bname = b.nodeString();
		SeqNode bnode = graph.getNode(bname);
		graph.remove(bnode);
	    }
	}

	// create graph & try untangle, given retryItems
	//   return if graph completed (may abort if circdep)
	private boolean createGraph() throws Xcept {
	    log("Creating MainGraph ..."); 
	    igraph = new MainGraph(this);
	    //if (logger().isVerbose("g"))
	        //dump(igraph);
	    nodeItems = igraph.nodeItems();

	    return untangle(igraph);
	}

	// untangler
	private boolean untangle(SeqGraph graph) throws Xcept {
	    SeqUntangler u3 = new SeqUntangler(graph, plan);
	    ugraph = u3;
	    u3.setMaxPulls(plan.seqMaxPulls());
	    u3.untangle();
	    SeqPath cpath = u3.getCircPath();
	    if (cpath != null) {
//	    	System.err.println("SeqUntangler: circpath=" + 
//		    u3.getCircPath());
		return !circPathRetry(u3, cpath);
	    }
	    logger().log("p", "Untangling completed with " + 
		    u3.npulls() + " pulls");
	    return true;
	}
	
	// check circ path for retry, 
	private boolean circPathRetry(SeqGraph graph, SeqPath cpath) 
	throws Xcept {    
	    boolean retry = false;
	    Iterator<SeqNode> cnodes = cpath.getNodes();
	    while (cnodes.hasNext()) {
	    	SeqNode cnode = cnodes.next();
		SeqItem item = nodeItems.get(cnode);
		if (retryItems.contains(item)) continue;
		if (item instanceof QueryTool) {
		    QueryTool qtool = (QueryTool) item;
		    if (! qtool.hasXexpr()) continue;
		    retryItems.add(item);
	    	    logger().warn("Positing delay: " + item);    
		    retry = true;
		} else if (item instanceof ImplicitBound) {
		    logger().warn("Implicit bound ignored: " + item);
		    retryItems.add(item);
		    retry = true;
		} else if (item instanceof TEvent) {
		    logger().warn("Strict sequencing disabled for: " + item);
		    retryItems.add(item);
		    retry = true;
		}    
	    }
	    if (retry) {
	    	logger().saveGraph("circretry", ugraph);
		return true;
	    }
	    logCircPath(graph, cpath);
	    throw new AbortXcept(
		 "Circular variable dependency: " + cpath);	    
	}

	// write circ path debug info to log
	private void logCircPath(SeqGraph graph, SeqPath path) 
	throws Xcept {
	    log("Circular path: " + path);
	    for (int i=0; i<path.nedges(); i++) {
	    	SeqNode node = path.edge(i).dest();
		SeqPhase phase = graph.getPhase(node);
		SeqItem item = nodeItems.get(node);
		log("\t" + node.name() + "\t@" + phase + "\t" + item);
	    }
	}

	// build phase into block
	private void buildPhaseBlock(SeqBlock block, SeqPhase phase) 
	throws Xcept {
	    log("building block for phase " + phase);
	    storeFFTNodes(phase, phase.x());
	    while (true) {
	    	int lsize = block.size();
		buildBlockNodes(block, phase);
		if (block.size() > lsize) continue;
		for (int i=0; i<phase.subphases.size(); i++) {
		    SeqPhase xphase = phase.subphases.get(i);
		    if (wphases.contains(xphase)) continue;
		    SeqEdge edge = entryEdge(xphase, xphase);
		    log("  subphase " + xphase + " entryEdge=" + edge);
		    if (edge != null) continue;
		    Domain x = model.domain(xphase.x());
	    	    SeqBlock xblock = new SeqBlock.Loop(block, x);
		    buildPhaseBlock(xblock, xphase);
		    if (xblock.size() == 0)
		    	log("  Repressing empty block: " + xblock);
		    else
		    	block.add(xblock);
		}
		if (block.size() == lsize) break;
	    }
	    LinkedHashSet<SeqNode> fftNodes = phaseFFTNodes.get(phase);
	    addFFTNodes(block, fftNodes);
	    ArrayList<SeqNode> wnodes = wgraph.getPhaseNodeArray(phase);
	    if (wnodes.size() > 0) throw new Xcept(
	    	"Phase " + phase + " block incomplete, missing " +
		wnodes);
	    for (int i=0; i<phase.subphases.size(); i++) {
	    	SeqPhase sphase = phase.subphases.get(i);
		if (! wphases.contains(sphase)) throw new Xcept(
		"Phase " + phase + " block incomplete, missing phase " +
		sphase);
	    }
	    wphases.add(phase);
	    // log("completed phase " + phase);
	}

	// store FFTs nodes for phase and subphases for one domain
	private void storeFFTNodes(SeqPhase phase, String x) 
	throws Xcept {
	    if (x == null) return;
	    LinkedHashSet<SeqNode> ffts = new LinkedHashSet<SeqNode>();
	    ArrayList<SeqNode> nodes = wgraph.getPhaseNodeArray(phase);
	    for (int i=0; i<nodes.size(); i++) {
	    	SeqNode node = nodes.get(i);
		if (node.fft() == null) continue;
		if (! node.fft().equals(x)) continue;
		SeqItem item = nodeItems.get(node);
		if (item instanceof Tool) {
		    ffts.add(node);
		    wgraph.remove(node);
		}
	    }
	    // log("storeFFTNodes: phase=" + phase +  " x=" + x + " nodes=" + ffts);
	    LinkedHashSet<SeqNode> pffts = phaseFFTNodes.get(phase);
	    if (pffts == null)
	    	phaseFFTNodes.put(phase, ffts);
	    else
	    	pffts.addAll(ffts);
		
	    for (int i=0; i<phase.subphases.size(); i++)
	    	storeFFTNodes(phase.subphases.get(i), x);	    
	}

	// add nodes into block
	private void buildBlockNodes(SeqBlock block, SeqPhase phase)
	throws Xcept {
	    ArrayList<SeqNode> nodes = wgraph.getPhaseNodeArray(phase);
	    for (int i=0; i<nodes.size(); i++) {
	    	SeqNode node = nodes.get(i);
		if (wgraph.nedgesTo(node) > 0) continue;
		SeqItem item = nodeItems.get(node);
		block.add(item);
		wgraph.remove(node);
	    }
	}

	// return phase entry edge, or none
	private SeqEdge entryEdge(SeqPhase tphase, SeqPhase phase) {
	    Iterator<SeqNode> nodes = wgraph.getPhaseNodes(phase);
	    while (nodes.hasNext()) {
	    	SeqNode node = nodes.next();
		Iterator<SeqEdge> edges = wgraph.getEdgesTo(node);
		while (edges.hasNext()) {
		    SeqEdge edge = edges.next();
		    SeqNode src = edge.src();
		    SeqPhase sphase = wgraph.getPhase(src);
		    if (! tphase.contains(sphase))
		    	return edge;
		}
	    }
	    for (int i=0; i<phase.subphases.size(); i++) {
	    	SeqEdge edge = entryEdge(tphase,
		    phase.subphases.get(i));
		if (edge != null) return edge;
	    }
	    return null;
	}
	    
	// add FFTs to end of phase block
	private void addFFTNodes(SeqBlock block, 
	LinkedHashSet<SeqNode> fftNodes) throws Xcept {
	    if (fftNodes == null || fftNodes.size() == 0) return;
	    Iterator<SeqNode> nodes = fftNodes.iterator();
	    ArrayList<MuBlock> dblocks = new ArrayList<MuBlock>();
	    while (nodes.hasNext()) {
	    	SeqNode node = nodes.next();
		SeqItem item = nodeItems.get(node);
		if (item instanceof DETool) {
		    DETool detool = (DETool) item;
		    MuBlock mblock = matchingMuBlock(block, detool, dblocks);
		    mblock.addDE(detool);
		} else
		    block.add(item);
	    }
	    for (int i=0; i<dblocks.size(); i++) {
	        MuBlock dblock = dblocks.get(i);
	    	block.add(dblock);
		deblocks.add(dblock);
	    }
	}	    

	// get matching MuBlock from list, or create new
	private MuBlock matchingMuBlock(SeqBlock parent, DETool tool,
	ArrayList<MuBlock> dblocks) throws Xcept {
	    for (int i=0; i<dblocks.size(); i++) {
	    	MuBlock deblock = dblocks.get(i);
		if (deblock.matches(tool))
		    return deblock;
	    }
	    MuBlock deblock = (tool.xs.size() == 0) ?
	    	new ODEBlock(parent, tool, vuTools) :
		new PDEBlock(parent, tool, vuTools);
	    dblocks.add(deblock);
	    return deblock;
	}

	// query
	public String title() { return "main"; }
	public Hashtable<VarUsage, Tool> vuTools() { return vuTools; }
	private static Hashtable<Var, DomainSet> hackVarReqDoms
	    = new Hashtable<Var, DomainSet>();
	public Hashtable<Var, DomainSet> varReqDoms() {
	    if (seqMem == null) return hackVarReqDoms;
	    return seqMem.varReqDoms();
	}
	    
	
	//// LOGGING MSGS / DEBUG
	public Logger logger() { return plan.logger; }
	public void log(String msg) { logger().log(msg); }

	// log main sequencables
	private void logItems() throws Xcept {
	    log("Tools to sequence:");
	    for (int i=0; i<tools.size(); i++) 
	        log(tools.get(i));
	    log("Events to seqence:");
	    for (int i=0; i<events.size(); i++) 
	    	log(events.get(i));
	    log("Relations to sequence:");
	    for (int i=0; i<relations.size(); i++) 
	    	log(relations.get(i));
	}	    

	// log sequencable
	private void log(SeqItem seq) throws Xcept {
	    DomainSet loops = seq.seqLoops();
	    VarUsages vups = new VarUsages(model);
	    if (seq instanceof Tool)
	    	vups = ((Tool) seq).vsols;
	    if (seq instanceof TEvent)
	    	vups = ((TEvent) seq).vacts;
	    String sloops = loops.atString();
	    if (sloops.length() > 0) sloops = sloops + " ";
	    log("  " + sloops + seq +
		" :: " + vups + "<<" + seq.vreqs());
	}	    

	// dump graph
	private void dump(SeqGraph graph) throws Xcept {
	    if (logger().out != null)
	    	SeqIO.write(graph, logger().out);
	}

	// query
	public Hashtable<SeqNode, SeqItem> nodeItems() { return nodeItems; }
	public SeqGraph untangledGraph() { return ugraph; }
	

}


	    
