// write java code for an implicit solver

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JImplicitWriter extends JProblemWriter {
	protected ImplicitTool tool;

	// constructor
	public JImplicitWriter(JMethodWriter parent, 
	ImplicitTool tool, String idSfx) throws Xcept {
	    super(parent, tool);
	    this.tool = tool;
	    if (parent instanceof JMainWriter) 
	    	id = "";
	    else
	        id = parent.id + idSfx + "__";
	    id = id + vname(tool.vsols.get(0).v());
	    for (int i=0; i<n(); i++) 
	    	cache.add(vsol(i).v());
	}

	// write solve call
	public void writeSolverCall() throws Xcept {
	    String obj = problemObject();
	    JWriter pp = parent.parent;
	    if (pp instanceof JODEWriter) 
	    	obj = ((JODEWriter) pp).problemObject();
	    if (pp instanceof JPDEWriter) 
	    	obj = ((JPDEWriter) pp).problemObject();
	    println(obj + ".solve(this);");
	}

	// write problem class
	public void writeClass() throws Xcept {

	    println("// Implicit block solving " + tool.vsols);
	    println("public class " + problemClass() +
	    	" extends " + extendsClass() + " {");
	    indentIn();
	    println("public " + problemClass() + 
	    	"(RTModel m) throws Xcept {");
	    indentIn();
	    println("super(m, " + Q + problemClass() + Q + 
	        C + n() + ");");
	    indentOut();
	    println("}");

	    if (isLinear()) 
	    	writeCtxtCall("setCoef", "mat,vec,rec",
	    	    "double[],double[],double[]");
	    writeCtxtCall("setBounds", "vmin,vmax,vguess",
	    	"double[],double[],double[]");
//	    writeCtxtCall("calcZero", "val,zero",
//	    	"double[],double[]");
	    startMethod("calcZero", "ctxt,val,zero", 
	       "RTContext,double[],double[]");
	    println("((XContext) ctxt).calcZero__" + id + 
	    	"(val,zero);");
	    println("ncallbacks++;");
	    stopMethod();
	    writeCtxtCall("export", "val",
	    	"double[]");

	    indentOut();
	    println("}");
	    println(S);
	}

	// write ctxt method
	public void writeMethods() throws Xcept {
	    println("// ctxt methods for " + problemClass());
	    if (isLinear())
	        writeSetCoef();

	    // set bounds
	    startMethod("setBounds__" + id, "vmin,vmax,vguess",
	    	"double[],double[],double[]");
	    for (int i=0; i<n(); i++) {
	    	VarUsage vu = vsol(i);
		writeBound(vu, ImplicitBound.MIN, "vmin[" + i);
		writeBound(vu, ImplicitBound.APPROX, "vguess[" + i);
		writeBound(vu, ImplicitBound.MAX, "vmax[" + i);
	    }
	    stopMethod();

	    // calcZero
	    startMethod("calcZero__" + id, "vals,zeroes", "double[],double[]");
	        for (int i=0; i<n(); i++) {
		    Var v = vsol(i).v();
		    println(vcache(v) + "=vals[" + i + "];");
		}
		for (int i=0; i<tool.vsols.size(); i++) {
	    	TExpr expr = tool.exprs.get(i).zeroExpr();
		println("zeroes[" + i + "]=" + 
		    fmt(expr.expr().simplify()) + ";");
	    }
	    stopMethod();

	    // export
	    startMethod("export__" + id, "vals", "double[]");
	    for (int i=0; i<n(); i++) {
	    	String sexpr = "vals[" + i + "]"; 
		Var v = vsol(i).v();
		String vcache = parentVcache(v);
		if (vcache != null) 
		    sexpr = vcache + "=" + sexpr;
		if (! isMu)
		    sexpr = "set(" + vstruct(v) + C + sexpr + ")";
		if (isMu && vcache == null)
		    sexpr = "// " + v + 
		    " not exported: mu-stepped & unused by parent";
		println(sexpr + ";");
	    }
	    stopMethod();
	}

	// write bound, if available
	public void writeBound(VarUsage vu, int type, String vname) 
	throws Xcept {
	    ImplicitBound b = tool.getBound(vu, type);
	    if (b == null) return;
	    String sexpr = fmt(b.expr().expr());
	    println(vname + "]=" + sexpr + ";");
	}
	
	// write ctxt linear methods
	public void writeSetCoef() throws Xcept {
	    startMethod("setCoef__" + id, "mat,vec,rec",
	    	    "double[],double[],double[]");
	    for (int i=0; i<n(); i++) {
	    	for (int j=0; j<n(); j++) {
		    int k=i*n()+j;
		    println("mat[" + k + "]=" + 
		    	fmt(tool.linmat(i, j).expr()) + ";");
		}
	    }
	    for (int i=0; i<n(); i++) 
	    	println("vec[" + i + "]=" +
		    fmt(tool.linmat(i, n()).expr()) + ";");
	    stopMethod();
	}

	// queries
	public boolean isLinear() { return tool.isLinear(); }
	public int n() { return tool.vsols.size(); }
	public VarUsage vsol(int i) { return tool.vsols.get(i); }
	public String extendsClass() {
	    return isLinear() ? "Fzero1Problem" : "Fzero2Problem";
	}
	   
}

