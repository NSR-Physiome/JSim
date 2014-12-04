/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// cost function for evaluation of ODE/PDE block

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class MuCost {
	private Hashtable<Object, Double> itemCosts;

	// ODE/PDE n vs cb/c:  fit A*n + B
	private static final double RADAU_A = 1;
	private static final double RADAU_B = 17;
	private static final double CVODE_A = .927;
	private static final double CVODE_B = 18.35;
	private static final double TOMS731_A = 835;
	private static final double TOMS731_B = 2214;

	// linear implicit: NAUX = A * NIMP + B
	private static final double LINZERO_A = 1.12;
	private static final double LINZERO_B = 21.9;
	
	// non-linear implicit: NAUX = A * NIMP^2 + B * NIMP + C
	private static final double GGOPT_A = 6.6;
	private static final double GGOPT_B = -15.89;
	private static final double GGOPT_C = 101.6;
	
	// fixed costs
	private static final double EXTERN_TOOL_COST = 1;
	private static final double MISC_TOOL_COST = 1;
	private static final double VAR_EXPR_COST = 1;
	private static final double CONST_EXPR_COST = 1;
	private static final double OP_EXPR_COST = 1;

	// constructor
	private MuCost() {
	    itemCosts = new Hashtable<Object,Double>();
	}

	// cost of a cluster
	public double calcCost(MuCluster cluster) throws Xcept {
	    double cbcost = 0;
	    Iterator<Tool> toolIter = cluster.toolIter();
	    while (toolIter.hasNext()) 
	    	cbcost += getCost(toolIter.next());
	    Iterator<DECon> destateIter = cluster.destateIter();
	    while (destateIter.hasNext())
	    	cbcost += getCost(destateIter.next());
	    return calcBlockCost(cluster.ndes(), cbcost, cluster.isPDE());
	}

	// cost of 2 clusters combined
	public double calcCost(MuCluster c1, MuCluster c2) throws Xcept {
	    if (c1.ntools() < c2.ntools()) {
	    	MuCluster temp = c1;
		c1 = c2;
		c2 = temp;
	    }
	    double cbcost = c1.cbcost();
	    int ndes = c1.ndes();
	    Iterator<Tool> toolIter = c2.toolIter();
	    while (toolIter.hasNext()) {
		Tool tool = toolIter.next();
		if (c1.hasTool(tool)) continue;
	    	cbcost += getCost(tool);
		if (tool instanceof DETool) ndes++;
	    }
	    Iterator<DECon> destateIter = c2.destateIter();
	    while (destateIter.hasNext()) {
	    	DECon con = destateIter.next();
		if (c1.hasDEState(con)) continue;
	    	cbcost += getCost(con);
	    }
	    return calcBlockCost(ndes, cbcost, c1.isPDE());
	}
	
	// get cost
	private double getCost(Object item) throws Xcept {
	    Double dd = itemCosts.get(item);
	    if (dd != null) return dd.doubleValue();
	    double d = (item instanceof Tool) ?
	    	calcCost((Tool) item) : calcCost((DECon) item);
	    dd = new Double(d);
	    itemCosts.put(item, dd);
	    return d;
	}
	
	// calc cost of DECon
	private double calcCost(DECon con) throws Xcept {
	    if (con.isEqn())
	    	return calcCost(con.eqn().cexpr());
	    else
	        return calcCost(con.tool());
	}

	// per callback relative cost in mublock
	private double calcCost(Tool tool) throws Xcept {
	    if (tool instanceof ExprTool) 
	    	return exprCost((ExprTool) tool);
	    if (tool instanceof DETool)
	    	return 0; // cluster uses DECons instead
	    if (tool instanceof ExternTool)
	    	return EXTERN_TOOL_COST;
	    if (tool instanceof ImplicitTool)
	    	return implicitCost((ImplicitTool) tool);
	    return MISC_TOOL_COST;
	}

	// calc cost of Expr Tool
	private double exprCost(ExprTool tool) throws Xcept {
	    Expr expr = tool.expr.expr();
	    return VAR_EXPR_COST + calcCost(expr);
	}
	    
	// calc cost of Implicit Tool
	private double implicitCost(ImplicitTool tool) throws Xcept {
	    int nimp = tool.exprs().size();
	    double naux = tool.isLinear() ? 
	    	LINZERO_A*nimp + LINZERO_B :
		GGOPT_A*nimp*nimp + GGOPT_B*nimp + GGOPT_C;
	    double naux2 = naux*(naux+1)/2;
	    double auxcost = 2*VAR_EXPR_COST + 2*OP_EXPR_COST;
	    return naux2 * auxcost;
	}
	    
	// calc cost of Expr
	private double calcCost(Expr expr) throws Xcept {
	    if (expr instanceof ConstExpr) return CONST_EXPR_COST;
	    if (expr instanceof NamedExpr) return VAR_EXPR_COST;
	    if (expr instanceof IExpr) {
	    	double cost = OP_EXPR_COST;
		// perhaps adjust OP_EXPR_COST depending upon operator?
		IExpr iexpr = (IExpr) expr;
		for (int i=0; i<iexpr.nargs(); i++) 
		    cost += calcCost(iexpr.arg(i));
		return cost;
	    }
	    throw new Xcept("MuCost doesn't support expr class " 
	    	+ expr.getClass());
	}

	// ODE/PDE block cost
	private double calcBlockCost(int ndes, double cbcost, boolean
	isPDE) {
	    double ncbs = isPDE ? 
	    	TOMS731_A * ndes + TOMS731_B :
		RADAU_A * ndes + RADAU_B;
	    return ncbs * cbcost;
	}
}


