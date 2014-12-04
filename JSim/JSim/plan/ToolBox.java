/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// collection of tools for a model

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ToolBox {
	public Plan plan;
	public TModel model;
	public ArrayList<Tool> tools;
	private LinkedHashSet<Var> knownEntire;
	private VarUsages knownPartial;
	private Hashtable<Var, Tool> partialTools;
	private Hashtable<TSubDom, LinkedHashSet<TEqn>> usedEqns;
	protected Hashtable<Var, Tool> mainTools;
	protected Hashtable<Object, DECon> deconMap; // TEqns/Tools forming DECons
	protected DerivBuilder derivBuilder;
	private boolean isWorking; // work flag

	// constructor
	public ToolBox(Plan plan) throws Xcept {
	    this.plan = plan;
	    this.model = plan.model();
	    tools = new ArrayList<Tool>();
	    knownEntire = new LinkedHashSet<Var>();
	    knownPartial = new VarUsages(model);
	    partialTools = new Hashtable<Var, Tool>();
	    mainTools = new Hashtable<Var, Tool>();
	    deconMap = new Hashtable<Object, DECon>();
	    usedEqns = new Hashtable<TSubDom, LinkedHashSet<TEqn>>();
	    usedEqns.put(model.entire, new LinkedHashSet<TEqn>());
	    derivBuilder = new DerivBuilder(this);
	}

	// build all the tools for this box
	protected void build() throws Xcept {
	    log("\nToolbox phase starting ...");
	    
	    // create ExternTools, DomainTools, StateTools
	    for (int i=0; i<model.vars.size(); i++) {
	    	Var v = model.vars.get(i);
		if (v.isExtern())
		    addTool(new ExternTool(model, v));
		else if (v.isState())
		    addTool(new StateTool(model, v));
		else if (v.isDomain())
		    addTool(new DomainTool(model, v));
	    }

	    // create ProcTools
	    for (int i=0; i<model.procs.size(); i++)
	    	addTool(new ProcTool(model, model.procs.get(i)));
	    
	    // main tool build loop
	    isWorking = true;
	    while(isWorking && !allVarsSolved()) {
	    	isWorking = false;
	    	buildCycle1();
		if (isWorking) continue;
		if (allVarsSolved() || allEqnsUsed()) break;
		buildCycle2();  
		if (isWorking) continue;
		buildCycle3();
		if (isWorking) continue;
		buildCycle4();
	    }
	    log("Toolbox phase - all cycles completed");

	    // missing domaux's set to externs
	    if (plan.allowMissingDomainControls()) {
	    	for (int i=0; i<model.ndomains(); i++) {
	    	    Domain x = model.doms.get(i);
		    setExtern(x.vmin);
		    setExtern(x.vmax);
		    setExtern(x.vdelta);
		}
	    }

	    // missing IC's set to externs
	    if (plan.allowMissingICs()) {
	    }

	    // orphan IC's set vars to constant
	    if (plan.allowOrphanICs()) {
	    }

	    // explicit message for incomplete tools
	    for (int i=0; i<tools.size(); i++) {
	        Tool tool = tools.get(i);
		if (tool.isComplete()) continue;
		String msg = (tool instanceof DETool) ?
		    ((DETool) tool).incompleteMessage() :
		    "state variable IC missing";
		logger().error(msg);
	    }
		
	    // check vars not entirely solved
	    ArrayList<Var> unsolvedVars = new ArrayList<Var>();
	    for (int i=0; i<model.vars.size(); i++) {
	    	Var v = model.vars.get(i);
		if (v.isDeriv()) continue;
		if (isKnown(v)) continue;
		unsolvedVars.add(v);
	    }
	    if (unsolvedVars.size() > 0)
		logger().error("Could not entirely solve variable(s) " +  unsolvedVars);

	    // check unused equations
	    int code = plan.ignoreUnusedEquations() ? 
	    	Logger.WARN : Logger.ERROR;
	    for (int i=0; i<model.eqns.size(); i++) {
	    	TEqn eqn = model.eqns.get(i);
		if (isUsed(eqn, model.entire)) continue;
		String msg = (unsolvedVars.size() == 0) ?
		    "Equation unneeded, overspecifies model" :
		    "Equation could not be processed";
		logger().log(code, msg + ": " + eqn);
	    }

	    // create IC parms, posit PDE 1st spatial deriv
	    for (int i=0; i<tools.size(); i++) {
	    	Tool tool = tools.get(i);
		if ((tool instanceof DETool) && plan.makeDEICParms())
		    tool.createICParms();
		if ((tool instanceof StateTool) && plan.makeStateICParms())
		    tool.createICParms();
		if (tool instanceof DETool) 
		    ((DETool) tool).positFirstSpatialDerivs(this);
	    }

	    // create PDEFactors, assuming no errors so far
	    if (logger().errors.size() == 0) {
	    	for (int i=0; i<tools.size(); i++) {
	    	    if (! (tools.get(i) instanceof DETool)) continue;
		    DETool detool = (DETool) tools.get(i);
		    detool.createFactors();
		    derivBuilder.makePDEFactorTools(detool);
		}
	    }

	}

	// set unsolved var to extern
	private void setExtern(Var v) throws Xcept {
	    if (isKnown(v)) return;
	    addTool(new ExternTool(model, v));
	    logger().warn("Unconstraint variable "  + v + " set to extern");
	}

	// all equations used entirely?
	private boolean allEqnsUsed() {
	    for (int i=0; i<model.eqns.size(); i++) 
	    	if (! isUsed(model.eqns.get(i), model.entire))
		    return false;
	    return true;
	}

	// all vars solved entire + IC vars solved in IC boundary?
	private boolean allVarsSolved() {
	    for (int i=0; i<model.vars.size(); i++) 
	    	if (! isKnown(model.vars.get(i)))
		    return false;
	    return true;
	}

	// build cycle: 1-var eqns, State/DETool constraints
	private void buildCycle1() throws Xcept {
	    log("Toolbox phase - cycle 1:");
	    for (int i=0; i<model.eqns.size(); i++) 
		buildOneVarEqn(model.eqns.get(i), model.entire); 
	    for (int i=0; i<tools.size(); i++) 
		buildToolDECon(tools.get(i));
	    for (int i=0; i<model.vars.size(); i++) 
	    	 buildDerivTool(model.vars.get(i));
	}
	
	// build cycle: implicit eqns
	private void buildCycle2() throws Xcept {
	    log("Toolbox phase - cycle 2:");
	    ImplicitPool pool = new ImplicitPool(this);
	    ImplicitTool tool = pool.makeTool(model.entire);
	    if (tool == null) return;
	    addTool(tool);
	    for (int i=0; i<tool.eqns.size(); i++) 
	    	setUsed(tool.eqns.get(i), model.entire);
	}

	// build cycle: DECon from eqns
	private void buildCycle3() throws Xcept {
	    log("Toolbox phase - cycle 3:");
	    for (int i=0; i<model.eqns.size(); i++) {
		TEqn eqn = model.eqns.get(i);
		buildEqnDECon(eqn, model.entire);
	    }
	}

	// build cycle: u:x(x.min/x.max) is a BC
	private void buildCycle4() throws Xcept {
	    log("Toolbox phase - cycle 4:");
	    for (int i=0; i<model.eqns.size(); i++) {
		TEqn eqn = model.eqns.get(i);
		buildEqnBC(eqn, model.entire);
		if (isWorking) break;
	    }
	}	    

	// build ExprTool or Implicit from 1-unknown var eqns ?	
  	private void buildOneVarEqn(TEqn eqn, TSubDom sd) 
	throws Xcept {
	    if (isUsed(eqn, sd)) return;
	    VarUsages vus = eqn.usages();
	    if (! sd.isEntire()) vus = vus.restrict(sd);
	    vus = unknown(vus, 2);
	    log("  check eqn: " + eqn + "; " + vus);
	    if (vus.nvars() > 1) return;
	    if (vus.size() < 1) 
		throw new AbortXcept(
		    "Equation overspecifies model: " + eqn);
	    VarUsage vu = vus.get(0);  // vu to solve
	    if (vus.size() > 1 || !vu.isSolvable()) 
	        return;
	    TExpr vexpr = eqn.expr().solveFor(vu.v());
	    Tool tool = (vexpr != null) ?
	    	new ExprTool(vu, vexpr) : 
		new ImplicitTool(vu, eqn.expr());
	    tool.eqns.add(eqn);
	    addTool(tool);
	    setUsed(eqn, sd);
	}

	// build DECon for DETools from existing tool
	private void buildToolDECon(Tool tool) throws Xcept {
	    if (tool.needsICs()) return;
	    if (deconMap.get(tool) != null) return;
	    VarUsages vus = deconVarUsages(tool.vsols);
	    if (vus.size() < 1) return;
	    DECon con = new DECon(vus, tool);
	    addDECon(con);
	    deconMap.put(tool, con);
	}
 
	// build DECon from Eqn
	private void buildEqnDECon(TEqn eqn, TSubDom sd) 
	throws Xcept {
	    if (isUsed(eqn, sd)) return;
	    VarUsages vus = eqn.usages();
	    if (! sd.isEntire()) vus = vus.restrict(sd);
 	    vus = unknown(vus, vus.size());
	    vus = deconVarUsages(vus);
	    if (vus.size() < 2) return; // 1 vu -> tool, not eqn
	    DECon con = new DECon(vus, eqn);
	    if (con.isBC() && con.v0s().size() > 2) return;
	    addDECon(con);
	    deconMap.put(eqn, con);
	    setUsed(eqn, sd);

	    // if Eqn is ODE state tool, add to ToolBox
	    Tool tool = con.tool();
	    if (tool != null) {
	    	addTool(tool);
		deconMap.put(tool, con);
	    }
	}

	// build BC for any eqn with u:x(x.min/x.max) 
	private void buildEqnBC(TEqn eqn, TSubDom sd)
	throws Xcept {
	    if (isUsed(eqn, sd)) return;
	    VarUsages vus = eqn.usages();
	    if (! sd.isEntire()) vus = vus.restrict(sd);
 	    vus = unknown(vus, vus.size());
	    vus = deconVarUsages(vus);
	    VarUsage bcvu = null;
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (! vu.isBoundary()) continue;
		Var vx = vu.v();
		if (vx.derivOrder() != 1) continue;
		if (vu.domain() != vx.derivDomain()) continue;
		if (model.pureChildDerivDoms.get(vx.zeroDeriv()).size() < 2) continue;
		if (bcvu != null) return;
		bcvu = vu;
	    }
	    if (bcvu == null) return;
	    
	    // create DECon for bcvu
	    vus = new VarUsages(model);
	    vus.add(bcvu);
	    DECon con = new DECon(vus, eqn);
	    addDECon(con);
	    deconMap.put(eqn, con);
	    setUsed(eqn, sd);
	}
	
	// select unknown vus with DECon potential
    	private VarUsages deconVarUsages(VarUsages rawvus) 
	throws Xcept {
	    VarUsages vus = new VarUsages(model);
	    for (int i=0; i<rawvus.size(); i++) {
	    	VarUsage vu = rawvus.get(i);
		if (! vu.isSolvable()) continue;
		Var v = vu.v();
		Var v0 = v.zeroDeriv();
		Tool v0tool = mainTools.get(v0);
		boolean vok = (v0tool instanceof DETool) ||
		    ((v0tool == null) && model.hasDeriv(v0));
		if (! vok) continue;
		boolean vuok = vu.isBoundary() || v.isDeriv();
		if (! vuok) continue;
		vus.add(vu);
	    }
	    return vus;
	}

	// add DECon to DETool(s), create if needed
	private void addDECon(DECon con) throws Xcept {
	    for (int i=0; i<con.v0s().size(); i++) {
	    	Var v = con.v0s().get(i);
		DETool detool = (DETool) mainTools.get(v);
		if (detool == null) {
		    detool = new DETool(model, v);
		    addTool(detool);
		}
		log("  updated " + detool.deString() + 
		    "(" + v + ") with " + con);
 		detool.addCon(con);
		toolUpdated(detool);
	    }
	}

	// build DerivTool for unknown vx with v known
	private void buildDerivTool(Var vx) throws Xcept { 
	    if (! vx.isDeriv()) return;
	    if (isKnown(vx)) return;
	    Var v = vx.unDeriv();
	    Tool tool = mainTools.get(v);
	    if (tool == null) return;
	    if (tool instanceof DETool) return; // hack, really
	    if (! tool.isComplete()) return;
	    Domain x = vx.derivDomain();
	    Tool xtool = tool.derivTool(x);
	    if (xtool == null) throw new AbortXcept(
	    	"Can't solve " + vx + " because can't differentiate "
		+ tool + " with respect to " + x);
	    if (! plan.makeDerivTools()) throw new AbortXcept(
	    	"Symbolic derivatives not enabled: " + xtool);
	    addTool(xtool);
	}

	//// Toolbox bookkeeping

	// add tool to this ToolBox
	protected void addTool(Tool tool) throws Xcept {
	    log("  add tool: " + tool);
	    tools.add(tool);
	    toolUpdated(tool);
	    buildStateIC(tool);
	}
	
	// tool updated
	protected void toolUpdated(Tool tool) throws Xcept {
	    setKnown(tool);
	    for (int i=0; i<tool.vsols.size(); i++) {
	    	VarUsage vu = tool.vsols.get(i);
		Var v = vu.v();
		if (! vu.isBoundary())
		    mainTools.put(v, tool);
	    }
	    updateDomainAux(tool);
	    isWorking = true;	
	}

	// update knownEntire, knownPartial for tool
	private void setKnown(Tool tool) throws Xcept {
	    VarUsages vus = tool.vsols;
	    boolean isComp = tool.isComplete();
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		Var v = vu.v();
		if (vu.isCurr() && isComp) {
		    knownEntire.add(v);
		    if (tool.needsICs())
		    	log("    " + v + " is now entirely solved");
		    else 
		    	checkKnownPartialConflict(tool, v);
		} else {
		    knownPartial.add(vu);
		    partialTools.put(v, tool);
		}
	    }
	}

	// non-IC tool solves v entirely, check if overspec
	private void checkKnownPartialConflict(Tool tool, Var v)
	throws Xcept {
//	    log("    check overspec: " + tool + " for var " + v);
//	    if (! knownPartial.hasVar(v)) return; // old version s/b same
	    Tool ptool = partialTools.get(v);
	    if (ptool == null) return;
	    logger().warn(
	    	"" + ptool + ptool.lineNoMessage() + " overspecifies variable " + v
		+ " which is completely specified by " + tool + tool.lineNoMessage());
	}

	// is v entire known?
	protected boolean isKnown(Var v) {
	    return knownEntire.contains(v);
	}

	// is vu known?
	protected boolean isKnown(VarUsage vu) {
	    Var v = vu.v();
	    if (knownEntire.contains(v)) return true;
	    if (knownPartial.contains(vu)) return true;
	    switch (vu.stat()) {
	    case VarUsage.MIN:
	    case VarUsage.MAX:
	   	return false;
	    default:
	    	return knownPartial.hasCurr(v);
	    }
	}
	
	// are vus known?
	protected boolean isKnown(VarUsages vus) {
	    for (int i=0; i<vus.size(); i++)
	    	if (! isKnown(vus.get(i))) return false;
	    return true;
	}

	// get VarUsage's not implied by known, up to max #vars
	public VarUsages unknown(VarUsages src, int maxNVar)
	throws Xcept {
	    VarUsages vus = new VarUsages(model);
	    for (int i=0; i<src.size(); i++) {
	    	VarUsage vu = src.get(i);
		if (isKnown(vu)) continue;
		vus.add(vu);
		if (vus.nvars() >= maxNVar) break;
	    }
	    return vus;
	}

	// build StateTool ICs from existing tool
  	private void buildStateIC(Tool tool) throws Xcept {
	    if (tool.needsICs()) return;
	    for (int i=0; i<tool.vsols.size(); i++) {
	    	VarUsage vu = tool.vsols.get(i);
		if (! vu.isSolvable()) continue;
		if (! vu.isBoundary()) continue;
		Var v = vu.v();
		if (v.isDeriv()) continue;
		if (! (mainTools.get(v) instanceof StateTool))
		    continue;
		StateTool stool = (StateTool) mainTools.get(v);
		stool.setIC(tool);
		toolUpdated(stool);
	    }
	}	    

	// update domain aux vars as solved
	private void updateDomainAux(Tool tool) throws Xcept {
	    for (int i=0; i<tool.vsols.size(); i++) {
	    	Var xaux = tool.vsols.get(i).v();
		Domain x = xaux.auxForDomain();
		if (x == null) continue;
		Tool mtool = mainTools.get(x);
		if (! (mtool instanceof DomainTool)) throw new Xcept(
		    "No DomainTool for " + x);
		DomainTool domtool = (DomainTool) mtool;
		ExprTool auxtool = domtool.addAux(xaux);
		if (auxtool == null) continue;
		if (isKnown(auxtool.vsols))  throw new AbortXcept(
		    "Domain " + x + " overspecified by " + tool);
		addTool(auxtool);
	    }
	}

	// eqn is used?
	protected boolean isUsed(TEqn eqn, TSubDom sd) {
	    LinkedHashSet<TEqn> eset = usedEqns.get(model.entire);
	    if (eset.contains(eqn)) return true;
	    if (sd.isEntire()) return false;
	    eset = usedEqns.get(sd);
	    if (eset == null) return false;
	    return eset.contains(eqn);
	}
	protected void setUsed(TEqn eqn, TSubDom sd) {
	    LinkedHashSet<TEqn> eset = usedEqns.get(sd);
	    if (eset == null) { 
	        eset = new LinkedHashSet<TEqn>();
		usedEqns.put(sd, eset);
	    }
	    eset.add(eqn);
	}

	// sequence tool set
	public LinkedHashSet<Tool> seqTools() {
	    ArrayList<Tool> mtools = new ArrayList<Tool>(
	    	mainTools.values());
	    LinkedHashSet<Tool> tset = new LinkedHashSet<Tool>(mtools);
	    for (int i=0; i<mtools.size(); i++) {
	    	Tool mtool = mtools.get(i);
		if (mtool.icTools == null) continue;
		tset.addAll(mtool.icTools);
	    }
	    return tset;
	}

	// entire PDE 1st spatial deriv?
	protected boolean isPDEFirstSpatialDeriv(VarUsage vu) throws Xcept {
	    if (! vu.isCurr()) return false;
	    Var v = vu.v();
	    if (v.derivOrder() != 1) return false;
	    Var v0 = v.zeroDeriv();
	    if (! (mainTools.get(v0) instanceof DETool))
	    	return false;
	    DETool detool = (DETool) mainTools.get(v0);
	    Domain x = v.derivDomain();
	    return detool.xs.contains(x);
	} 

	// is input tool? (user settable at runtime)
	public boolean isInput(Tool tool) {
	    if (! plan.makeInputTools()) return false;
	    if (tool instanceof ExternTool) return true;
	    if (! (tool instanceof ExprTool)) return false;
	    ExprTool xtool = (ExprTool) tool;
	    if (xtool.vreqs.size() > 0) return false;
	    if (! xtool.vu.isCurr()) return false;
	    Var v = xtool.vu.v();
	    if (v.isDeriv()) return false;
	    if (v.isPrivate()) return false;
	    return true;
	}

	//// LOGGING MSGS
	public Logger logger() { return plan.logger; }
	public void log(String msg) { logger().log(msg); }

}

	    
