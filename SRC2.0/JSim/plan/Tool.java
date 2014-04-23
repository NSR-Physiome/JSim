/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one tool (mechanism for solving one or more VarUsages)

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public abstract class Tool implements SeqItem {
	public TModel model;
	public VarUsages vsols; // usages solved by tool
	public VarUsages vreqs; // usages required by tool
	public DomainSet seqLoops; // sequence/calculation loops
	public ArrayList<TEqn> eqns; // eqns used as basis for tool
	public LinkedHashSet<Tool> icTools; // ICs for DE & StateTools
	
	// static constants
	public static final int QUERY = VarUsage.QUERY;
	public static final int CURR = VarUsage.CURR;
	public static final int MIN = VarUsage.MIN;
	public static final int MAX = VarUsage.MAX;
	public static final int XEXPR = VarUsage.XEXPR;
	public static final int DELAY = VarUsage.DELAY;
	public static final int EXPR = VarUsage.EXPR;

	// constructor
	public Tool(TModel model) throws Xcept {
	    this.model = model;
	    vsols = new VarUsages(model);
	    vreqs = new VarUsages(model);
	    eqns = new ArrayList<TEqn>();
	}

	// set sequence loops field, check for domain conflicts
	protected void setSeqLoops() throws Xcept {
	    // check all vsols have same seqLoops
	    VarUsage vu0 = null;
	    for (int i=0; i<vsols.size(); i++) {
	        VarUsage vu = vsols.get(i);
	    	DomainSet vloops = vu.seqLoops();
		if (seqLoops == null) {
		    vu0 = vu;
		    seqLoops = vloops;
		}
		if (seqLoops.equals(vloops)) continue;
		logger().error("Can't sequence " + this + 
		    ": solved vars " + vu0 + seqLoops
		    + " and " + vu + vloops + 
		    " have incompatible domains.");
		return;
	    }

	    // check vreqs 
	    DomainSet rloops = new DomainSet();
	    for (int i=0; i<vreqs.size(); i++) {
	        VarUsage vu = vreqs.get(i);
		vu.addSeqLoops(rloops);
		rloops.removeAll(seqLoops);
		if (rloops.size() == 0) continue;
		logger().error("Can't sequence " + this + 
		    ": source var " + vu + 
		    " has free domain(s) " + rloops +
		    " missing in solved var(s): " + vsols);
		return;
	    }
	}

	// create IC parameters (DETool & StateTool)
	protected void createICParms() throws Xcept {
	    ArrayList<Tool> tools = new ArrayList<Tool>(icTools);
	    icTools = new LinkedHashSet<Tool>();
	    for (int i=0; i<tools.size(); i++) 
	    	icTools.add(createICParm(tools.get(i)));
	}

	// create IC parameter tool, if possible
	private Tool createICParm(Tool tool) throws Xcept {

	    // check if really can create IC parm
	    if (! (tool instanceof ExprTool)) return tool;
	    ExprTool xtool = (ExprTool) tool;
	    VarUsage vu = xtool.vu;
	    Var v = vu.v();
	    if (v.isPrivate()) return tool;
	    TExpr expr = xtool.expr;
	    if (expr.usages().size() > 0) return tool;
	    String vinitName = v.toString() + "__init";
	    vinitName = vinitName.replace(':', '_');
	    if (model.var(vinitName) != null) return tool;
	    
	    // create IC parm v__init
	    Expr.List doms = new Expr.List();
	    for (int i=0; i<v.ndim(); i++) {
		Domain x = v.domain(i);
		if (x != vu.domain())
		    doms.add(x);
	    }
	    Var vinit = v.isInt() ?
	    	(Var) new IntNVar(model.math, vinitName, doms) :
	    	(Var) new RealNVar(model.math, vinitName, doms);
	    Unit unit = v.unit();
	    if (unit != null) vinit.setUnit(unit);
	    model.updateVarMaps();

	    // add vic tool to ToolBox
	    VarUsage vinitu = new VarUsage(model, vinit);
	    expr = new TExpr(model, expr.expr()); // remove when(t=t.min) from expr
	    Tool vinitTool = new ExprTool(vinitu, expr);
	    model.plan.box().addTool(vinitTool);

	    // return modified icTool
	    TExpr tvinit = new TExpr(model, vinit);
	    return new ExprTool(vu, tvinit);
	}

	// DETool & StateTool ICs
	protected boolean needsICs() { return false; }
	protected boolean isComplete() { return true; }

	// take derivative (extend in subclasses)
	protected Tool derivTool(Domain x) throws Xcept { return null; } 

	// query
	public Logger logger() { return model.logger(); }
	public void log(String msg) { logger().log(msg); }
	public Domain t() { return null; } // DEtool, STateTool override
	public DomainSet seqLoops() { return seqLoops; }
	public VarUsages vreqs() { return vreqs; }
	public String nodeString() { return vsols.nodeString(); }
	abstract public String toolType();

	// line# message
	public String lineNoMessage() {
	    if (eqns.isEmpty()) return "";
	    XceptPos codeRef = eqns.get(0).codeRef();
	    if (codeRef == null) return "";
	    return " near line " + codeRef.lineno;
	}
}
