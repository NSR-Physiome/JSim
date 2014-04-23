// Expr formatter

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JExprWriter extends JWriter {
  	protected JExprWriter parent; 
	protected LinkedHashSet<Var> cache;
	protected int cacheLevel;
	protected boolean isMu;  // this level a mu-cache

	// constructor
	public JExprWriter(JExprWriter parent) {
	    super();
	    this.parent = parent;
	    cache = new LinkedHashSet<Var>();
	    if (parent != null) {
	    	cacheLevel = parent.cacheLevel + 1;
	    	isMu = parent.isMu;
	    }
	}

	// simple query
	public JPlanWriter planWriter() { return parent.planWriter(); }
	public State state() { return planWriter().state(); }

	// cache methods
	public String vcache(Var v) {
	    if (cache.contains(v))
	        return vcache(v, cacheLevel);
	    return parentVcache(v);
	}

	public String parentVcache(Var v) {
	    if (parent != null)
	        return parent.vcache(v);
	    return null;
	}

	public boolean isMuCached(Var v) {
	    if (cache.contains(v)) return isMu;
	    if (parent != null) 
	    	return parent.isMuCached(v);
	    return false;
	}

	// Expr formatting
	public String fmt(Expr expr) throws Xcept {
	    if (expr instanceof Var) {
	    	return fmt1((Var) expr);
	    } else if (expr instanceof IExpr) {
	    	return fmt1((IExpr) expr);
	    } else if (expr instanceof RealConst) {
	    	return fmt1((RealConst) expr);
	    } else if (expr instanceof VarFuncCall) {
	    	return fmt1((VarFuncCall) expr);
	    } else if (expr instanceof XFuncCall) {
	    	return fmt1((XFuncCall) expr);
	    } else if (expr instanceof IntegralIF) {
	    	return fmt1((IntegralIF) expr);
	    } else if (expr == null) {
	    	throw new Xcept("JExprWriter null expr");
	    } else {
	        throw new Xcept(
	    	    "JExprWriter doesn't support " + expr
		    + " class=" + expr.getClass());
	    }		
	}
	
	// Var strings
	private String fmt1(Var v) {
	    String s = vcache(v);
	    return (s == null) ? vcurr(v) : s;
	}
	
	// IExpr string
	private String fmt1(IExpr expr) throws Xcept {
	    int op = expr.op();
	    String sop = jlang.chars(op);
	    int n = expr.nargs();
	    String[] sargs = new String[n];
	    for (int i=0 ; i<n; i++) 
	    	sargs[i] = fmt(expr.arg(i));
	    if (jlang.swapXY(op)) {
	    	String stemp = sargs[0];
		sargs[0] = sargs[1];
		sargs[1] = stemp;
	    }
	    
	    if (jlang.func(op)) 
	    	return fmt(sop, sargs);
	    if (n == 1)
	    	return sop + "(" + sargs[0] + ")";
	    else if (n == 2)
	    	return "(" + sargs[0] + sop + sargs[1] + ")";
	    else if (op == IExpr.IF) 
	    	return "((" + sargs[0] + ") ? (" + 
		    sargs[1] + ") : (" + sargs[2] + "))";
	    else throw new Xcept(
	    	"JWriter: no format for operator=" + op);
	}

	// const string
	private String fmt1(RealConst c) {
	    double d = c.realVal(null);
	    if (Double.isNaN(d)) return "Double.NaN";
	    String s = c.toString();
	    if (d<0) s = "(" + s + ")";
	    return s;
	}

	// VFC
	private String fmt1(VarFuncCall vfc) throws Xcept {
	    String buf = "realVal(" + vstruct(vfc.v) +
	        ", new double[] {";
	    for (int i=0; i<vfc.args.size(); i++) {
	    	if (i>0) buf = buf + ",";
		buf = buf + fmt(vfc.args.expr(i));
	    }
	    return buf + "})";
	}

	// XFuncCall
	private String fmt1(XFuncCall fc) throws Xcept {
	    return planWriter().fpWriter.funcCallName(fc)
	        + ".realVal(this)";
	}

	// IntegralIF
	private String fmt1(IntegralIF integ) throws Xcept {
	    return fmt(integ.evalExpr());
	}

}
		
	    	
