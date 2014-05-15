// write java code for a MuBlock

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JODEWriter extends JProblemWriter {
	protected ODEBlock block;
	protected ArrayList<Var> vodes; 
	protected JODEWriter[] splitWriters;

	// constructor
	public JODEWriter(JMethodWriter parent, ODEBlock block) 
	throws Xcept {
	    super(parent, block);
	    this.block = block;
	    id = "" + vname(block.vstate().get(0));
	    isMu = true;
	    cache.addAll(block.vmus().vset());
	    vodes = new ArrayList<Var>();

	    for (int i=0; i<block.detools().size(); i++) {
	    	DETool detool = block.detools().get(i);
		Var v = detool.v;
		for (int j=0; j<detool.torder; j++) {
		    vodes.add(v);
		    v = v.deriv(detool.t);
		}
	    }

	    addSubWriterBlock(block);

	    if (isSplit()) {
	    	int n = splitBlocks().length;
	    	splitWriters = new JODEWriter[n];
		for (int i=0; i<n; i++) {
		    ODEBlock splitBlock = splitBlocks()[i];
		    splitWriters[i] = new JODEWriter(this, splitBlock);
		}
	    }
	}

	// add cacheVars
	protected void addCacheVars(LinkedHashSet<JCacheVar> cacheVars)
	throws Xcept {
	    if (isSplit()) {
		for (int i=0; i<splitWriters.length; i++)
		    splitWriters[i].addCacheVars(cacheVars);
	    } else {
	    	super.addCacheVars(cacheVars);
	    }
	}

	// write problem (maybe array declaration
	public void writeDecl() throws Xcept {
	    if (isSplit()) 
	        println("public RTProblem[] " + problemObject() + ";");
	    else 
	    	super.writeDecl(); 
	}
	
	// write problem object instantiation
	public void writeInst() throws Xcept {
	    if (isSplit()) {
	    	int n = splitWriters.length;
	        println(problemObject() 
		    + " = new RTProblem[" + n + "];");
		for (int i=0; i<n; i++) {
		    println(problemObject() + "[" + i + "] = new " +
		    	splitWriters[i].problemClass() + "(this);");
		} 
	    } else {
	    	super.writeInst(); 
	    }
	}

	// write solver call
	public void writeSolverCall() throws Xcept {
	    if (isSplit()) 
	        println("solve(" + problemObject() + ");");
	    else 
	    	super.writeSolverCall(); 
	}
	

	// write problem class
	public void writeClass() throws Xcept {

	    // split blocks?
	    if (isSplit()) {
		for (int i=0; i<splitWriters.length; i++) 
		    splitWriters[i].writeClass();
		return;
	    }


	    // single block
	    println("// ODE problem solving " + block.vstate());
	    println("public class " + problemClass() +
	    	" extends " + extendsClass() + " {");
	    indentIn();
	    println("public " + problemClass() + 
	    	"(RTModel m) throws Xcept {");
	    indentIn();
	    println("super(m, " + Q + problemClass() + Q + ");");
	    String line = "setup(" + vstruct(block.t()) + 
	        ", new RTRealNVar[] {";
	    for (int i=0; i<vodes.size(); i++) {
		if (i>0) line = line + ",";
		line = line + vstruct(vodes.get(i));
	    }
	    line = line + "});";
	    println(line);
	    indentOut();
	    println("}");
	    println("public void evaluate(RTContext ctxt, double t, "
	        + "double[] u, double[] udot) throws Xcept {");
	    indentIn();
	    println("((XContext) ctxt)." + evalMethod() +
	    	"(t, u, udot);");
	    println("interruptCheck(ctxt, t, udot);");
	    println("ncallbacks++;");
	    indentOut();
	    println("}");
	    indentOut();
	    println("}");
	    println(S);
	}

	// write ctxt method
	public void writeMethods() throws Xcept {

	    // split blocks? 
	    if (isSplit()) {
		for (int i=0; i<splitWriters.length; i++) 
		    splitWriters[i].writeMethods();
		return;
	    }

	    // single block
	    println("// ctxt ODE evaluate");
	    startMethod(evalMethod(), 
	    	"t,u,udot", "double,double[],double[]");

	    // load caches
	    println(vcache(block.t()) + " = t;");
	    for (int i=0; i<vodes.size(); i++) {
	    	Var v = vodes.get(i);
	    	println(vcache(v) + " = u[" + i + "];");
	    }

	    // block calculations
	    writeBlock(block);

	    // store caches
	    int j=0;
	    for (int i=0; i<block.detools().size(); i++) {
	    	DETool detool = block.detools().get(i);
		Var v = detool.v.deriv(block.t());
	    	println("udot[" + j++ + "] = " + fmt(v) + ";");
	 	for (int k=1; k<detool.torder; k++) {
		    v = v.deriv(block.t());
		    println("udot[" + j++ + "] = " 
		        + fmt(v) + ";");
		}
	    }

	    stopMethod();
	    println("");
	}

	// queries
	public String evalMethod() { return "evaluate__" + id; }
	public String extendsClass() { return "ODE1Problem"; }  
	public ODEBlock[] splitBlocks() { return block.splitBlocks(); }
	public boolean isSplit() { return splitBlocks() != null; }
}

