/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// symbolic derivative processing

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*; 
import JSim.aserver.*; 

import java.io.*;
import java.util.*;

public class DerivBuilder {
	private ToolBox box;
	
	// constructor
	public DerivBuilder(ToolBox box) {
	    this.box = box;
	}

	//  make deriv tools for PDE factors (currently Toms731 only)
	protected void makePDEFactorTools(DETool tool) throws Xcept {
	    if (! (tool.factors() instanceof PDE1Factors)) return;
	    PDE1Factors factors = (PDE1Factors) tool.factors();
	    if (factors.solverMsg(ASModel.PDE_Toms731) != null) return;
	    Expr coefD = factors.coefD();
	    Domain x = tool.xs.first();
	    log("  checking PDE(" + tool.v + 
	    	") D=" + coefD + " for differentiability ...");
	    String msg = makeDerivTools(coefD, x);
	    if (msg != null) {
		log("  D=" + coefD + 
		    " is not differentiable: " + msg);
	    	factors.setCoefDx(null);
	    } else {
	    	Expr coefDx = coefD.takeDeriv(x);
		log("  setting Dx=" + coefDx);
		TExpr tcoefDx = new TExpr(model(), coefDx);
		VarUsages vus = tcoefDx.usages();
		if (vus.size() > 0) {
		    log("  adding DETool vreq: " + vus);
		    tool.vreqs.add(vus);
		    tool.state().vreqs().add(vus);
		}
		factors.setCoefDx(coefDx);
	    }
	}

	// create deriv tools for Expr:Domain, if possible
	//   return null if OK,  else error msg
	protected String makeDerivTools(Expr expr, Domain x)
	throws Xcept {
	    TExpr texpr = new TExpr(model(), expr);
	    VarUsages vus = texpr.usages();
	    LinkedHashSet<Var> vset = new LinkedHashSet<Var>();
	    LinkedHashSet<Tool> toolset = new LinkedHashSet<Tool>();
	    String msg = addDerivTools(vus, x, vset, toolset);
	    if (msg != null) return msg;

	    // add new vars, tools to toolbox
	    Iterator<Var> vs = vset.iterator();
	    while (vs.hasNext()) {
	   	Var vx = vs.next().deriv(x);
		log("  adding variable " + vx);
	    }
	    Iterator<Tool> tools = toolset.iterator();
	    while (tools.hasNext()) {
	    	Tool tool = tools.next();
		Tool xtool = tool.derivTool(x);
		if (xtool == null) throw new Xcept(
		    "Error creating PDEFactor deriv:" + x
		    + " for " + tool);
		box.addTool(xtool);
	    }
	    return null;
	}			

	// accumulate list of vars & tools for creating deriv
	//    return null on success,  error msg on failure
	private String addDerivTools(VarUsages vus, Domain x,
	LinkedHashSet<Var> vset, LinkedHashSet<Tool> toolset)
	throws Xcept {
//	    if (true) return null; 
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (! vu.isCurr()) 
		    return "Non-current VarUsage " + vu;
		Var v = vu.v();
		if (vset.contains(v)) continue;
		if (! v.hasDomain(x)) continue;
		Tool vtool = box.mainTools.get(v);
		if (vtool == null)
		    return "No tool for var " + v;
		if (vtool instanceof StateTool) continue;
		if (vtool instanceof DomainTool) continue;
		boolean ok = (vtool instanceof ExprTool) 
		    || (vtool instanceof ImplicitTool);
		if (!ok) 
		    return "Can't differentiate " + vtool;
		vset.add(v);
		toolset.add(vtool);
		addDerivTools(vtool.vreqs, x, vset, toolset);
	    }
	    return null;
	}
    
	// simple query
	public TModel model() { return box.model; }
	public void log(String msg) { logger().log(msg); }
	public Logger logger() { return box.logger(); }
	public ArrayList<Tool> tools() { return box.tools; }
}
