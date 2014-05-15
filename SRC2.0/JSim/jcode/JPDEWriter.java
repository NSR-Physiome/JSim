// write java code for a PDE block - 1D only supported

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 
import JSim.aserver.*; 

import java.io.*;
import java.util.*;

public class JPDEWriter extends JProblemWriter {
	protected PDEBlock block;
	protected int n;
	protected Domain t;
	protected Domain x;
	protected ArrayList<Var> us, uts, uxs, uxts, uxxs;
	protected JPDEWriter[] splitWriters;

	protected static final int LSFEA = ASModel.PDE_LSFEA;
	protected static final int MacCormack = ASModel.PDE_MacCormack;
	protected static final int Toms731 = ASModel.PDE_Toms731;

	// constructor
	public JPDEWriter(JMethodWriter parent, PDEBlock block) 
	throws Xcept {
	    super(parent, block);
	    this.block = block;
	    id = "" + vname(block.vstate().get(0));
	    isMu = true;
	    if (block.nsolvers() == 0) throw new Xcept(
	    	"" + block + " - 2D & 3D PDE solvers are not yet available");
	    if (! doSolver(LSFEA)
	    &&  ! doSolver(MacCormack)
	    &&  ! doSolver(Toms731))
	    	throw new Xcept(
		"" + block + " can't be factored for any available PDE solver");
	    n = vstate().size();
	    t = block.t();
	    x = block.xs().first();

	    // var deriv lists
	    us = vstate();
	    uts = new ArrayList<Var>();
	    uxs = new ArrayList<Var>();
	    uxts = new ArrayList<Var>();
	    uxxs = new ArrayList<Var>();	
	    for (int i=0; i<n; i++) {
		uts.add(us.get(i).deriv(t));
		uxs.add(us.get(i).deriv(x));
		uxts.add(null);
		uxxs.add(uxs.get(i).deriv(x));
	    }

	    cache.addAll(uxs);
	    cache.addAll(block.vmus().vset());
	    cache.addAll(lhbcBlock().vmus().vset());
	    cache.addAll(rhbcBlock().vmus().vset());
	    addSubWriterBlock(block);
	    addSubWriterBlock(lhbcBlock(), "__LHBC");
	    addSubWriterBlock(rhbcBlock(), "__RHBC");


	    if (isSplit()) {
	    	int n = splitBlocks().length;
	    	splitWriters = new JPDEWriter[n];
		for (int i=0; i<n; i++) {
		    PDEBlock splitBlock = splitBlocks()[i];
		    splitWriters[i] = new JPDEWriter(this, splitBlock);
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
	    println("// PDE problem solving " + block.vstate());
	    println("public class " + problemClass() +
	    	" extends " + extendsClass() + " {");
	    indentIn();
	    println("public " + problemClass() + 
	    	"(RTModel m) throws Xcept {");
	    indentIn();
	    println("super(m, " + Q + problemClass() + Q + ");");

	    // setup() method
	    println("setup(" + 
	        vstruct(t) + C +  vstruct(x) + ",0,");
	    indentIn();
	    println(fmtArr(us) + ",");
	    println(fmtArr(uxts) + ","); // u:t not solved
	    println(fmtArr(uxts) + ","); // u:x not solved
	    println(fmtArr(uxts) + ",");
	    println(fmtArr(uxts) + ","); // u:x:x not solved
	    println("null, null);");
	    indentOut();
	    println("setSolvedVars(new ASVar[] {" + vstruct(us) + "});");
	    DomainSet loopDomains = block.loopDomains();
	    if (loopDomains.size() > 0) {
		ArrayList<Var> lvars = new ArrayList<Var>();
		lvars.addAll(loopDomains);
		println("loopDomains = new RTRealDomain[] {"
		    + vstruct(lvars) + "};");
	    }

	    indentOut();
	    println("}");

	    // solver support
	    println("public boolean usesPDESolver(int which) {");
	    indentIn();
	    println("switch(which) {");
	    println("case ASModel.PDE_LSFEA: return " + 
	    	doSolver(LSFEA) + ";");
	    println("case ASModel.PDE_MacCormack: return " + 
	    	doSolver(MacCormack) + ";");
	    println("case ASModel.PDE_Toms731: return " + 
	    	doSolver(Toms731) + ";");
	    println("default: return false;");
	    println("}");
	    indentOut();
	    println("}");

	    // context calls
	    if (doSolver(LSFEA) || doSolver(MacCormack)) {
	    	writeCtxtCall("common_LHB",
		    "t,us,f1s,f2s,f3s", 
		    "double,double[],double[],double[],double[]");
	    	writeCtxtCall("common_RHB",
		    "t,us,f1s,f2s,f3s", 
		    "double,double[],double[],double[],double[]");
	    }
	    if (doSolver(LSFEA)) {
	    	writeCtxtCall("LSFEA_tstep",
		    "t,x,us,uts", 
		    "double,double,double[],double[]");
	    	writeCtxtCall("LSFEA_xstep",
		    "t,x,us,uxcoefs,uxxcoefs", 
		    "double,double,double[],double[],double[]");
	    }
	    if (doSolver(MacCormack)) {
	    	writeCtxtCall("MacCormack_State",
		    "t,x,us,cxs,cxxs,srcs",
		    "double,double,double[],double[],double[],double[]");
	    }
	    if (doSolver(Toms731)) {
	    	writeCtxtCall("Toms731_State",
		    "t,x,us,uxs,cs,qs,rs",
		    "double,double,double[],double[],double[],double[],double[]");
	    	writeCtxtCall("Toms731_LHB",
		    "t,us,uxs,betas,gammas",
		    "double,double[],double[],double[],double[]");
	    	writeCtxtCall("Toms731_RHB",
		    "t,us,uxs,betas,gammas",
		    "double,double[],double[],double[],double[]");
	    }

	    indentOut();
	    println("}");
	    println("");
	}

	// write ctxt call w/ PDE ncallback increment
	public void writeCtxtCall(String name, String args, String
	types) throws Xcept {

	    // split blocks? 
	    if (isSplit()) {
		for (int i=0; i<splitWriters.length; i++) 
		    splitWriters[i].writeCtxtCall(name, args, types);
		return;
	    }

	    // single block
	    startMethod(name, "ctxt," + args, "RTContext," + types);
	    println("npdeCallbacks[ProfileData." + name + "]++;");
	    println("((XContext) ctxt)." + name + "__" + id + 
	    	"(" + args + ");");
	    stopMethod();
	}

	// format var array from list
	protected String fmtArr(ArrayList<Var> vs) throws Xcept {
	    StringBuffer buf = new StringBuffer("new RTRealNVar[] {");
	    for (int i=0; i<n; i++) {
		if (i > 0) buf.append(",");
	    	Var v = vs.get(i);
		buf.append((v==null) ? "null" : vstruct(v));
	    }
	    buf.append("}");
	    return buf.toString();
	}

	// write ctxt methods
	public void writeMethods() throws Xcept {

	    // split blocks? 
	    if (isSplit()) {
		for (int i=0; i<splitWriters.length; i++) 
		    splitWriters[i].writeMethods();
		return;
	    }

	    // single block

	    // LSFEA state methods
	    if (doSolver(LSFEA)) {
	    	startMethod("LSFEA_tstep" + "__" + id,
		    "t,x,us,uts", 
		    "double,double,double[],double[]");
		loadCacheTXU();
		writeBlock(block);
		for (int i=0; i<n; i++) 
		    println("uts[" + i + "]=" + 
		    	fmt(factors(i).coefS()) + ";"); 
		stopMethod();

	    	startMethod("LSFEA_xstep" + "__" + id,
		    "t,x,us,uxcoefs,uxxcoefs", 
		    "double,double,double[],double[],double[]");
		loadCacheTXU();
		writeBlock(block);
		for (int i=0; i<n; i++) { 
		    println("uxcoefs[" + i + "]=" + 
		    	fmt(factors(i).coefB()) + ";"); 
		    println("uxxcoefs[" + i + "]=" + 
		    	fmt(factors(i).coefD()) + ";"); 
		}
		stopMethod();
	    }

	    // MacCormack state method
	    if (doSolver(MacCormack)) {
	    	startMethod("MacCormack_State" + "__" + id,
		    "t,x,us,cxs,cxxs,srcs",
		    "double,double,double[],double[],double[],double[]");
		loadCacheTXU();
		writeBlock(block);
		for (int i=0; i<n; i++) { 
		    println("cxs[" + i + "]=" + 
		    	fmt(factors(i).coefB()) + ";"); 
		    println("cxxs[" + i + "]=" + 
		    	fmt(factors(i).coefD()) + ";"); 
		    println("srcs[" + i + "]=" + 
		    	fmt(factors(i).coefS()) + ";"); 
		}		
		stopMethod();
	    }

	    // Toms731 state method
	    if (doSolver(Toms731)) {
	    	startMethod("Toms731_State" + "__" + id,
		    "t,x,us,uxs,cs,qs,rs",
		    "double,double,double[],double[],double[],double[],double[]");
		loadCacheTXU();
		loadCacheUX();
		writeBlock(block);
		for (int i=0; i<n; i++) { 
		    println("cs[" + i + "]=" + 
		    	fmt(factors(i).toms731_C()) + ";"); 
		    println("qs[" + i + "]=" + 
		    	fmt(factors(i).toms731_Q()) + ";"); 
		    println("rs[" + i + "]=" + 
		    	fmt(factors(i).toms731_R()) + ";"); 
		}
		stopMethod();
	    }

	    // BC methods
	    writeMethods(true, lhbcBlock());
	    writeMethods(false, rhbcBlock());
	    println("");

	}

	// write ctxt methods for 1 BC
	private void writeMethods(boolean left, BCBlock bcblock)
	throws Xcept {
	    String sfx = left ? "LHB" : "RHB";
	    if (doSolver(LSFEA) || doSolver(MacCormack)) {
	    	startMethod("common_" + sfx + "__" + id,
		    "t,us,f1s,f2s,f3s", 
		    "double,double[],double[],double[],double[]");
		loadCacheTU();
		loadCacheX(left);
		writeBlock(bcblock);
		for (int i=0; i<n; i++) { 
		    println("f1s[" + i + "]=" + 
		    	fmt(factors(i).coefF1(left)) + ";"); 
		    println("f2s[" + i + "]=" + 
		    	fmt(factors(i).coefF2(left)) + ";"); 
		    println("f3s[" + i + "]=" + 
		    	fmt(factors(i).coefF3(left)) + ";"); 
		}
		stopMethod();
	    }
	    if (doSolver(Toms731)) {
	    	startMethod("Toms731_" + sfx + "__" + id,
		    "t,us,uxs,betas,gammas",
		    "double,double[],double[],double[],double[]");
		loadCacheTU();
		loadCacheUX();
		loadCacheX(left);
		writeBlock(bcblock);
		for (int i=0; i<n; i++) { 
		    println("betas[" + i + "]=" + 
		    	fmt(factors(i).toms731_Beta(left)) + ";"); 
		    println("gammas[" + i + "]=" + 
		    	fmt(factors(i).toms731_Gamma(left)) + ";"); 
		}
		stopMethod();
	    }
	}

	// print cache loads
	protected void loadCache(Var v, String s) throws Xcept {
	    println(vcache(v) + "=" + s + ";");
	}
	protected void loadCache(ArrayList<Var> vs, String s) throws Xcept {
	    for (int i=0; i<vs.size(); i++) {
	    	Var v = vs.get(i);
		if (v == null) continue;
		println(vcache(v) + "=" + s + "[" + i + "];");
	    }
	}
	protected void loadCacheTU() throws Xcept {
	    loadCache(t, "t");
	    loadCache(us, "us");
	}
	protected void loadCacheTXU() throws Xcept {	 
	    loadCacheTU();
	    loadCache(x, "x");
	}
	protected void loadCacheUX() throws Xcept {	 
	    loadCache(uxs, "uxs");
	}
	protected void loadCacheX(boolean left) throws Xcept {
	    Var vaux = left ? x.vmin : x.vmax;
	    println(vcache(x) + "=" + fmt(vaux) + ";");
	}

	// queries
	public ArrayList<Var> vstate() { return block.vstate(); }
	public BCBlock lhbcBlock() { return block.bcblock(x, true); }
	public BCBlock rhbcBlock() { return block.bcblock(x, false); }
	public boolean doSolver(int i) { 
	    return block.solverMsg(i) == null; 
	}
	public PDE1Factors factors(int i) { 
	    return (PDE1Factors) block.factors(i); 
	}
	public String evalMethod() { return "evaluate__" + id; }
	public String extendsClass() { return "PDE1Problem"; }  
	public PDEBlock[] splitBlocks() { return block.splitBlocks(); }
	public boolean isSplit() { return splitBlocks() != null; }
}

