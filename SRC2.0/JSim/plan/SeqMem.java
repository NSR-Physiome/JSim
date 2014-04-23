/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// update plan to optimize memory usage
//   determine minimum RT storage for proper calculation
//   insert MemFree items once variables no longer needed

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class SeqMem {
	private MainBlock main;
	private int wTopInx;
	private SeqItem wTopItem;
	private LoopStack wloopStack, wloopStackClone;
	private Hashtable<Var, CodePoint> varAllocPoints, varFreePoints;
	private Hashtable<VarUsage, LoopStack> vuCalcLoops;
	private Hashtable<VarUsage, Integer> vuCalcTopInxs;
	private Hashtable<Var, DomainSet> varReqDoms;	

	// constructor
	public SeqMem(MainBlock main) {
	    this.main = main;
	    varAllocPoints = new Hashtable<Var, CodePoint>();
	    varFreePoints = new Hashtable<Var, CodePoint>();
	    vuCalcLoops = new Hashtable<VarUsage, LoopStack>();
	    vuCalcTopInxs = new Hashtable<VarUsage, Integer>();
	    varReqDoms = new Hashtable<Var, DomainSet>();
	}
	
	// process entire plan
	public void process() throws Xcept {

	    wTopInx = 0;
	    wTopItem = null;
	    wloopStack = new LoopStack();
	    wloopStackClone = new LoopStack();
	    process(main, 1);

	    wTopInx = 0;
	    wTopItem = null;
	    wloopStack = new LoopStack();
	    wloopStackClone = new LoopStack();
	    process(main, 2);

	    dump(System.err);
	    addVarMems();
	}
	
	// process one SeqItem
	private void process(SeqItem item, int pass) throws Xcept {
//System.err.println("process(" + pass + ") " + wloopStack + ": " + item + 
//" class=" + item.getClass());
	    if (pass == 1)
	        process1(item, pass);
	    else
	        process2(item, pass);
	}
		
	// update code points, calc stacks
	private void process1(SeqItem item, int pass) throws Xcept {
	    updateCodePoints(item, item.vreqs());
	    if (item instanceof Tool) {
	    	Tool tool = (Tool) item;
		updateCodePoints(item, tool.vsols);
		updateCalcStacks(tool.vsols);
	    } else if (item instanceof MuBlock) {
	    	MuBlock mublock = (MuBlock) item;
		updateCodePoints(item, mublock.vmus());
		updateCalcStacks(mublock.vmus());
	    } else if (item instanceof TEvent) {
	    	TEvent event = (TEvent) item;
		updateCodePoints(item, event.vacts);
	    } else if (item instanceof TRelation) {
		// nothing
	    } else if (item instanceof SeqBlock) {
	    	processBlock((SeqBlock) item, pass);
	    } else throw new Xcept(
	    	"SeqMem.processCodePoints(): Unsupported item class: " 
		+ item + " class=" + item.getClass());
	}
		
	// update required domains
	private void process2(SeqItem item, int pass) throws Xcept {
	    if (item instanceof Tool) {
	    	Tool tool = (Tool) item;
		if (main.box.isInput(tool))
		    updateInputReqDoms(tool);
	    	updateReqDoms(item.vreqs());
	    } else if (item instanceof SeqBlock) {
	    	processBlock((SeqBlock) item, pass);
	    } 
	}

	// process all 
	private void processBlock(SeqBlock block, int pass) throws Xcept {
//System.err.println("process(" + pass + ") " + wloopStack + ": " + block + 
//" class=" + block.getClass());
	    if (block instanceof SeqBlock.Loop) {
	    	wloopStack.push((SeqBlock.Loop) block);
		wloopStackClone = wloopStack.clone();
	    }
	    for (int i=0; i<block.size(); i++) {
	    	SeqItem item = block.items().get(i);
		if (block == main) {
		    wTopItem = item;
		    wTopInx = i;
		}
	    	process(item, pass);
	    }
	    if (block instanceof SeqBlock.Loop) {
	    	wloopStack.pop();
		wloopStackClone = wloopStack.clone();
	    }
	}

	// update first and last codepoints for vu
	private void updateCodePoints(SeqItem item, VarUsages vus) throws Xcept {
//System.err.println("  process " + topInx + " " + wloopStack + ": " + item + 
//" class=" + item.getClass() + " vus=" + vus);
	    if (vus == null) return;
	    for (int i=0; i<vus.size(); i++) {
	    	Var v = vus.get(i).v();
		if (v.ndim() == 0) continue;
		if (v.isDomain()) continue;
		CodePoint cp = new CodePoint(wTopItem,item);
		if (varAllocPoints.get(v) == null)
		    varAllocPoints.put(v, cp);
		varFreePoints.put(v, cp);
	    }
	}	

	// update vuCalcLoops (where vus are calculated)
	private void updateCalcStacks(VarUsages vus) throws Xcept {
	    for (int i=0; i<vus.size(); i++) {
		VarUsage vu = vus.get(i);
//		if (! vu.isCurr() && ! vu.isMin()) throw new Xcept(
//		    "SeqMem: vsol is neither CURR nor MIN " + vu); 
		vuCalcLoops.put(vu, wloopStackClone);
		vuCalcTopInxs.put(vu, wTopInx);
		updateToolReqDoms(vu);
	    }
	    
	}

	// update req store doms based on tool sequence location
	//   any domain
	private void updateToolReqDoms(VarUsage vu) throws Xcept {
	    Var v = vu.v();
	    DomainSet sdoms = varReqDoms.get(v);
	    if (sdoms == null) {
	    	sdoms = new DomainSet();
		varReqDoms.put(v, sdoms);
	    }
	    for (int i=0; i<v.ndim(); i++) {
	    	Domain x = v.domain(i);
		if (vu.isMin(x)) continue; 
		if (wloopStack.hasDomain(x)) continue;
		if (sdoms.contains(x)) continue;
		sdoms.add(x);
	    }
	}

	// input tools must save all domains for vsol
	private void updateInputReqDoms(Tool tool) throws Xcept {
	    VarUsages vus = tool.vsols;
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		Var v = vu.v();
		DomainSet sdoms = varReqDoms.get(v);
		if (sdoms == null) {
		    sdoms = new DomainSet();
		    varReqDoms.put(v, sdoms);
		}
	    	for (int j=0; j<v.ndim(); j++) {
	    	    Domain x = v.domain(j);
		    if (vu.isMin(x)) continue; 
		    sdoms.add(x);
		}		    
	    }
	}

	// update store doms from vreqs
	private void updateReqDoms(VarUsages vus) throws Xcept {
	    for (int i=0; i<vus.size(); i++) {
		VarUsage vu = vus.get(i);

		// calcLS = loopStacks for when v calculated
		VarUsage vu1 = vu;
		if (! vu1.isCurr())
		    vu1 = new VarUsage(vu.model(), vu.v());
		LoopStack calcLS = vuCalcLoops.get(vu1);
		if (calcLS == null) throw new Xcept(
		    "SeqMem: no calc loops for " + vu1);
		
		// sdoms = store domains for vu.v()
		Var v = vu.v();
	    	DomainSet sdoms = varReqDoms.get(v);
	    	if (sdoms == null) {
	    	    sdoms = new DomainSet();
		    varReqDoms.put(v, sdoms);
	        }

		// domain must be stored if
		//   queried in different loop than calculated
	        for (int j=0; j<v.ndim(); j++) {
	    	    Domain x = v.domain(j);
		    if (sdoms.contains(x)) continue;
		    SeqBlock.Loop calcLoop = calcLS.loopFor(x);
		    if (calcLoop == null) continue; // already added
		    SeqBlock.Loop useLoop = wloopStack.loopFor(x);
		    int qstat = vu.qstat(x);
		    boolean addx = false;
		    switch (qstat) {
		    case VarUsage.CURR:
		    	if (useLoop == null) throw new Xcept(
			    "SeqMem: useLoop=null for " + vu + " domain " + x);
			addx = (calcLoop != useLoop);
			break;
		    case VarUsage.EXPR:
		    case VarUsage.XEXPR:
		    case VarUsage.DELAY:
		    case VarUsage.MAX:    // perhaps MAX can be smarter?
		    	addx = true;
			break;
		    case VarUsage.MIN:
		        addx = reqDomsMin(vu, x);		    			
		        break;
		    }
		    if (addx) {
		    	sdoms.add(x);
		    }
		}
	    }
	}

	// true if v(x.min) has tool and wtopInx < v tool topInx
	private boolean reqDomsMin(VarUsage vu, Domain x) throws Xcept {
	    Var v = vu.v();
	    if (! vu.isMin(x)) 
	    	vu = new VarUsage(vu.model(), v, VarUsage.MIN, x);
	    if (vuCalcTopInxs.get(vu) == null) return false;
	    vu = new VarUsage(vu.model(), v);
	    if (vuCalcTopInxs.get(vu) == null) throw new Xcept(
	    	"SeqMem: no calcTool for " + v);
	    int toolTopInx = vuCalcTopInxs.get(vu);
	    return wTopInx >= toolTopInx;
	}

	// add VarMems to main block
	public void addVarMems() {

	    // accumulate topItem free list
	    Hashtable<SeqItem, ArrayList<Var>> itemVarFrees = 
	    	accumVarMems(varFreePoints);
	    Hashtable<SeqItem, ArrayList<Var>> itemVarAllocs = 
	    	accumVarMems(varAllocPoints);
		
	    // add VarMems to MainBlock SeqItem list
	    LinkedList<SeqItem> items = new LinkedList<SeqItem>(main.items());
	    for (int i=0; i<items.size(); i++) {
	    	SeqItem item = items.get(i);
		ArrayList<Var> vs = itemVarAllocs.get(item);
		if (vs != null) {
		    VarMem vf = new VarMem(false, vs);
		    items.add(i, vf);
		    i++;
		}
		vs = itemVarFrees.get(item);
		if (vs != null) {
		    VarMem vf = new VarMem(true, vs);
		    items.add(++i, vf);
		}
	    }
	    ArrayList<SeqItem> nitems = new ArrayList<SeqItem>(items);
	    main.resetItems(nitems); 
	}

	// create accumulated VarMem list
	public Hashtable<SeqItem, ArrayList<Var>> accumVarMems(
	Hashtable<Var, CodePoint> points) {
	    Hashtable<SeqItem, ArrayList<Var>> itemVarMems = 
 		new Hashtable<SeqItem, ArrayList<Var>>();
	    Iterator<Var> viter = points.keySet().iterator();
	    while (viter.hasNext()) {
	    	Var v = viter.next();
		SeqItem topItem = points.get(v).topItem;
		ArrayList<Var> vs = itemVarMems.get(topItem);
		if (vs == null) {
		    vs = new ArrayList<Var>();
		    itemVarMems.put(topItem, vs);
		}
		vs.add(v);
	    }
	    return itemVarMems;
	}

	// LoopStack class
	public static class LoopStack extends ArrayList<SeqBlock.Loop> {
	    public LoopStack() { super(); }
	    public LoopStack clone() {
	    	return (LoopStack) super.clone();
	    }
	    public String toString() {
	        return super.toString() + "_" + hashCode();
	    }
	    public void push(SeqBlock.Loop block) {
	    	add(block);
	    }
	    public SeqBlock.Loop pop() throws Xcept {
	     	if (isEmpty()) throw new Xcept(
		     "LoopStack.pop(): empty stack");
		SeqBlock.Loop block = get(size()-1);
		remove(size()-1);
		return block;
	    }
	    public boolean hasDomain(Domain x) {
	    	return loopFor(x) != null;
	    }
	    public SeqBlock.Loop loopFor(Domain x) {
	    	for (int i=0; i<size(); i++) {
		    SeqBlock.Loop block = get(i);
		    if (block.x() == x) return block;
		}
		return null;
	    }     
	}

	// Code point class
	public static class CodePoint {
	    public SeqItem topItem;  // item in MainBlock
	    public SeqItem item;
	    public CodePoint(SeqItem topItem, SeqItem item) {
		this.topItem = topItem;
		this.item = item;
	    }
	    public String toString() {
	    	return "#" + topItem.hashCode() + "__" + item;
	    }
	}

	// dump
	private void dump(PrintStream out) {
	    /*out.println("==== var[Alloc/Free]Points");	    
	    Iterator<Var> viter = varFreePoints.keySet().iterator();
	    while (viter.hasNext()) {
	    	Var v = viter.next();
		out.println("  " + v + ": " +
		    varAllocPoints.get(v) + " :: " +
		    varFreePoints.get(v));
	    }*/
	    /*out.println("==== varReqDoms");	    
	    Iterator<Var> viter = varReqDoms.keySet().iterator();
	    while (viter.hasNext()) {
	    	Var v = viter.next();
		DomainSet vdoms = varReqDoms.get(v);
		if (vdoms.isEmpty()) continue;
		out.println("  " + v + ": " + vdoms);
	    }*/
	}

	// simple query
	public TModel model() { return main.model; }
	public Hashtable<Var, DomainSet> varReqDoms() { return varReqDoms; }
}
