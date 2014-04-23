/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// block of sequenced calculations

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class SeqBlock implements SeqItem {
	private TModel model;
	private ArrayList<SeqItem> items;
	protected DomainSet specDoms; // domains set for block calcs
	
	// constructors
	public SeqBlock(TModel model) throws Xcept {
	    this.model = model;
	    items = new ArrayList<SeqItem>();
	    specDoms = new DomainSet();
	}	
	public SeqBlock(SeqBlock parent) throws Xcept {
	    this.model = parent.model;
	    items = new ArrayList<SeqItem>();
	    specDoms = new DomainSet(parent.specDoms);
	}
	
	// add item to block
	public void add(SeqItem item) {
	    model.log("  adding " + item + " to " + this);
	    items.add(item);
	}
	
	// dump block to out
	public void dump(PrintStream out) {
	    PrintWriter wout = new PrintWriter(out, true);
	    dump(wout);
	}
	public void dump(PrintWriter out) { dump(this, out, ""); }
	public void dump(SeqBlock block, PrintWriter out, 
	String indent) {
	    String hdr = indent + block.title();
	    if (block instanceof MuBlock) 
	    	hdr = hdr + " vmus=" + ((MuBlock) block).vmus;
	    out.println(hdr);
	    indent = indent + "  ";
	    if (indent.length() > 12) {
	    	out.println("DUMP ABORTED: TOO MANY INDENTS");
		return;
	    }
	    for (int i=0; i<block.items.size(); i++) {
	    	SeqItem item = block.items.get(i);
		if (item instanceof PDEBlock) 
		    dumpPDE((PDEBlock) item, out, indent);
		else if (item instanceof SeqBlock) 
		    dump((SeqBlock) item, out, indent);
		else
		    out.println(indent + item);
	    }
	}
	
	// dump PDE block
	public void dumpPDE(PDEBlock block, PrintWriter out,
	String indent) {
	    dump(block, out, indent);
	    Iterator<Domain> xs = block.xs.iterator();
	    while (xs.hasNext()) {
		Domain x = xs.next();
		dump(block.bcblock(x, true), out, indent + "  ");
		dump(block.bcblock(x, false), out, indent + "  ");
	    }
	}
	
	// reset items
	//   used only in MainBlock after SeqMem processing
	protected void resetItems(ArrayList<SeqItem> items) {
	    this.items = items;
	}

	// query
	public TModel model() { return model; }
	public String title() { return "untitled"; }
	public String toString() { return title(); }
	public ArrayList<SeqItem> items() { return items; }
	public int size() { return items.size(); }
	public Domain t() { return null; }
	public VarUsages vreqs() { return null; }
	public DomainSet seqLoops() { return null; }
	public String nodeString() { return null; }

	// Loop over domain
	public static class Loop extends SeqBlock {
	    private Domain x;  // loop domain
	    public Loop(SeqBlock parent, Domain x) throws Xcept {
	    	super(parent);
		this.x = x;
		specDoms.add(x);
	    }
	    public Domain x() { return x; }
	    public String title() { return "loop-" + x; }
	}
}
