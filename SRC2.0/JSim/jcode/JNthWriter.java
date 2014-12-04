// Java output for RunTime Nth point support by domain

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JNthWriter extends JWriter {
	protected JPlanWriter pwrt;
	private LinkedHashSet<String> badDoms; // unsupported runtime Nth doms

	// constructor
	public JNthWriter(JPlanWriter pwrt) {
	    this.pwrt = pwrt;
	    badDoms = new LinkedHashSet<String>();
	    try {
	    	process();
	    } catch (Exception e) {
	    	System.err.println("JNthWriter error: " + e);
		e.printStackTrace();
	    }
	    audit("JNthWriter: badDoms=" + badDoms);
	}

	// build badDoms
	protected void process() throws Xcept {
	    process(pwrt.mainBlock());
	}

	// process one SeqItem
	protected void process(SeqItem item) throws Xcept {
	    if (item instanceof SeqBlock)
	        processBlock((SeqBlock) item);
	    if (item instanceof ProcTool)
	        processProc((ProcTool) item);
	    else
	    	processItem(item);
	}
	
	// process one SeqBlock
	protected void processBlock(SeqBlock block) throws Xcept {
	    ArrayList<SeqItem> items = block.items();
	    for (int i=0; i<items.size(); i++)
	    	process(items.get(i));
	    if (block instanceof ODEBlock)
	    	processODEBlock((ODEBlock) block);
	    if (block instanceof PDEBlock)
	    	processPDEBlock((PDEBlock) block);
	}

	// ODE blocks
	protected void processODEBlock(ODEBlock block) throws Xcept {
	    processExtraDoms(block, 1);
	}

	// PDE spatial doms are badDoms
	protected void processPDEBlock(PDEBlock block) throws Xcept {
	    Iterator<Domain> iter = block.xs().iterator();
	    while (iter.hasNext()) {
	    	Domain x = iter.next();
		addBadDom(x);
	    }	    
	    processExtraDoms(block, 2);
	}

	// tag ODE/PDEblocks with extra doms -> all doms bad
	private void processExtraDoms(MuBlock block, int nmax) 
	throws Xcept {
	    Var v = block.vstate().get(0);
	    if (v.ndim() <= nmax) return;
	    for (int i=0; i<v.ndim(); i++)
	    	addBadDom(v.domain(i));
	}	

	// process ProcTool
	protected void processProc(ProcTool tool) throws Xcept {
	    XFuncArg.List args = tool.fc().args();
	    for (int i=0; i<args.size(); i++) {
	    	XFuncArg arg = args.get(i);
		Expr.List argDoms = arg.argDoms();
		for (int j=0; j<argDoms.size(); j++) {
		    Expr x = argDoms.get(j);
		    if (x instanceof Domain)
		    	addBadDom((Domain) x);
		}
	    }
	    processItem(tool);
	}

	// process one SeqItem (not SeqBlock)
	protected void processItem(SeqItem item) throws Xcept {
	    VarUsages vreqs = item.vreqs();
	    if (vreqs == null) return;
	    for (int i=0; i<vreqs.size(); i++) {
	    	VarUsage vu = vreqs.get(i);
		if (vu.isCurr()) continue;
		if (vu.isMin()) continue; // bears further study???
		Var v = vu.v();
		for (int j=0; j<v.ndim(); j++) {
		    Domain x = v.domain(j);
		    //int qstat = vu.qstat(x);
		    // if (qstat == VarUsage.CURR) continue;
		    // if (qstat == VarUsage.MIN) continue;
		    //audit("JNthWriter: " + item.getClass() + " :: " + 
		    //	item + " vu=" + vu + " dom=" + x);
		    addBadDom(x);
	    	}
	    }
	}

	// add Domain to badDoms
	private void addBadDom(Domain x) {
	    badDoms.add(x.name());
	}

	// write RTModel.runTimeNthSupported(RTRealDomain x) method
	protected void writeNth() throws Xcept {
	    println("// every Nth point support");
	    println("public boolean runTimeNthSupported(RTRealDomain x) {");
	    indentIn();
	    if (badDoms.size() > 0)
	    	println("String xname = x.name();");
	    Iterator<String> iter = badDoms.iterator();
	    while (iter.hasNext()) {
	    	String xname = iter.next();
		println("if (xname.equals(\"" + xname + "\")) return false;");
	    }
	    println("return true;");
	    indentOut();
	    println("}");
	    println("");
	}

	// audit msgs to stderr
	private void audit(String s) {
	    // System.err.println(s);
	}

	// simple query
	public State state() { return pwrt.state(); }
}
