/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// how variables used in an expression

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class TExpr {
	private TModel model;
	private TSubDom subdom; // restriction of expr, if not entire
	private Expr expr;
	private VarUsages usages;

	// static constants
	public static final int QUERY = VarUsage.QUERY;
	public static final int CURR = VarUsage.CURR;
	public static final int MIN = VarUsage.MIN;
	public static final int MAX = VarUsage.MAX;
	public static final int DELAY = VarUsage.DELAY;
	public static final int XEXPR = VarUsage.XEXPR;
	public static final int EXPR = VarUsage.EXPR;

	// constructor
	public TExpr(TModel model, Expr expr) throws Xcept {
	    this.model = model;
	    subdom = model.entire;
	    this.expr = expr;
	    usages = new VarUsages(model);
	    loadExpr(new Map(), expr);
//System.err.println("TExpr: " + expr + " vus=" + usages);
	}
	private TExpr(TModel model) {
	    this.model = model;
	}

	// restrict to subdom
	protected TExpr restrict(TSubDom subdom) throws Xcept {
	    if (subdom.isEntire()) return this;
	    if (! variesWith(subdom.domain(), expr))
	    	return this;
	    TExpr texpr = new TExpr(model);
	    texpr.subdom = subdom;
	    texpr.expr = expr;
	    texpr.usages = usages.restrict(subdom);
	    return texpr;
	}
		
	// load usages/units from Expr
	private void loadExpr(Map map, Expr expr) throws Xcept {
	    if (expr instanceof RealConst)
	    	loadRealConst((RealConst) expr);
	    else if (expr instanceof ConstExpr)
	    	return;
	    else if (expr instanceof IExpr)
	    	loadIExpr(map, (IExpr) expr);
	    else if (expr instanceof Var)
	    	loadVar(map, (Var) expr);
	    else if (expr instanceof VarFuncCall)
	    	loadVarFuncCall(map, (VarFuncCall) expr);
	    else if (expr instanceof XFuncCall)
	    	loadXFuncCall(map, (XFuncCall) expr);
	    else if (expr instanceof IntegralIF)
	    	loadIntegral(map, (IntegralIF) expr);
	    else throw new Xcept(expr,
	    	"Unsupported TExpr class: " + expr.getClass());
	}

	// load units from real constants
	private void loadRealConst(RealConst expr) throws Xcept {
	   if (expr.unit() != null)
	   	model.addConstUnit(expr.unit());
	}
	
	// load Expr with restricted unsolvable map
	private void loadExpr(Map map, Expr expr, Domain x, Expr xsub) 
	throws Xcept {
	    map = new Map(map);
	    map.put(x, xsub);
	    map.setUnsolvable();
	    loadExpr(map, expr);
	}	    	

	// load usages from IExpr
	private void loadIExpr(Map map, IExpr expr) throws Xcept {
	    if (expr.op() == IExpr.FORALL) {
	    	Expr arg = expr.arg(0);
		Domain x = (Domain) expr.arg(1);
		loadExpr(map, arg, x, x.vmin);
		loadExpr(map, arg, x, x.vmax);
	    } else for (int i=0; i<expr.nargs(); i++) {
	    	loadExpr(map, expr.arg(i));
	    }
	}
	
	// load usages from Var
	private void loadVar(Map map, Var v) throws Xcept {
	    int[] qstat = new int[v.ndim()];
	    for (int i=0; i<qstat.length; i++) {
	    	Domain x = v.domain(i);
		Expr expr = map.get(x);
		if (expr == null) expr = x;
		qstat[i] = qstat(v, x, expr);
	    }
	    VarUsage vu = new VarUsage(model, v, qstat);
	    if (! map.isSolvable()) vu.setUnsolvable();
	    usages.add(vu);
	}
	
	// load usages from VarFuncCall
	private void loadVarFuncCall(Map map, VarFuncCall vfc) throws Xcept {
	    Var v = vfc.v;
	    Map nmap = new Map(map);
	    for (int i=0; i<v.ndim(); i++) {
	    	Domain x = v.domain(i);
		Expr argx = vfc.args.get(i);

		// substitute old map values for domains, if any
		Expr xexpr = map.get(x);
		if (xexpr != null) {
		    Expr.List list1 = new Expr.List(1);
		    list1.add(x);
		    Expr.List list2 = new Expr.List(1);
		    list2.add(xexpr);
		    argx = argx.replace(list1, list2);
		}

		loadExpr(map, argx);	
		if (argx != x) 
		    nmap.put(x, argx);		    
	    }
	    loadVar(nmap, v);
	}

	// VarFuncCall usage status
	private int qstat(Var v, Domain x, Expr expr) 
	throws Xcept {
	    if (expr == x) return CURR;
	    if (expr == x.vmin) return MIN;
	    if (expr == x.vmax) return MAX;
	    if (isDelay(x, expr)) return DELAY;
	    return variesWith(x, expr) ? XEXPR : EXPR;
	}
	
	// delay line = x - something
	private boolean isDelay(Domain x, Expr expr) 
	throws Xcept {
	    if (expr == x) return true;
	    if (expr == x.vmin) return true;
	    if (! (expr instanceof IExpr)) return false;
	    IExpr iexpr = (IExpr) expr;
	    if (iexpr.op() == IExpr.SUB)
	    	return iexpr.arg(0) == x;
	    if (iexpr.op() == IExpr.IF)
	    	return isDelay(x, iexpr.arg(1))
		   && isDelay(x, iexpr.arg(2));
	    return false;
	}

	// does Expr vary with Domain?  
	// addDomains usage NEEDS REWORKING ???
  	public boolean variesWith(Domain x, Expr expr) throws Xcept {
  	    Expr.List doms = new Expr.List();
	    expr.addDomains(doms);
	    return doms.containSame(x);
	}

	// load usages from XFuncCall 
	private void loadXFuncCall(Map map, XFuncCall fc) throws Xcept {
	    for (int i=0; i<fc.ninputs(); i++) {
	    	Expr expr = fc.args().arg(i).orig();
	    	loadExpr(map, expr);
	    }
	}

	// load Integral or Summation
	public void loadIntegral(Map map, IntegralIF summ) throws Xcept {
	    loadExpr(map, summ.evalExpr());
	}

	// load Integral or Summation
	public void loadIntegralNEW(Map map, IntegralIF summ) throws Xcept {
	    loadIntegral(map, summ.u(), summ.t(), summ.min());
	    loadIntegral(map, summ.u(), summ.t(), summ.max());
//	    loadExpr(map, summ.evalExpr());
	}

	// one bound of Integral/Summation
	private void loadIntegral(Map map, Expr base, Domain t, Expr
	bound) throws Xcept {
	    Expr.List tList = new Expr.List(1);
	    tList.add(t);
	    Expr.List boundList = new Expr.List(1);
	    boundList.add(bound);
	    Expr expr = base.replace(tList, boundList);
//System.err.println("loadIntegral " + base + " -> " + expr);
	    loadExpr(map, expr);
	}

	// solve expr for VarUsage, return null if fails
	protected TExpr solveFor(Var tv) throws Xcept {
	    NamedExpr v = tv;
	    if (! (expr instanceof CompareExpr)) return null;	    
	    CompareExpr bexpr = (CompareExpr) expr;
	    try {
	    	Expr nexpr = Algebra.solve(v, bexpr.arg(0), bexpr.arg(1));
		TExpr rexpr = new TExpr(model, nexpr);
		return rexpr.restrict(subdom);
	    } catch (Xcept e) {
		return null;
	    }
	}

	// take deriv 
	protected TExpr deriv(Domain x) throws Xcept {
	    Expr xexpr = expr.takeDomDeriv(x);
	    xexpr = xexpr.expandDeriv(); // may create vars
	    xexpr = xexpr.simplify();
	    model.updateVarMaps();
	    return new TExpr(model, xexpr);
	}

	// extract linear factors of vu from this TExpr
	//   keep=true -> factor of vu,  keep=false -> remained of vu
	protected TExpr linearFactor(VarUsage vu, boolean keep)
	throws Xcept {
	    NamedQueryExpr qexpr = 
	    	factorUsingV(vu) ? vu.v() : vu.qexpr();
	    Expr lexpr = expr.linearFactor(qexpr, keep).simplify();
	    TExpr rexpr = new TExpr(model, lexpr);
	    rexpr = rexpr.restrict(subdom);
//model.log("expr=" + expr +" subdom=" + subdom +
//" qexpr=" + qexpr + " vu=" + vu + " keep="
//+ keep + " lexpr=" + lexpr + " rexpr=" + rexpr);
	    return rexpr;
	}	       

	// factor using v from vu if vu domains match subdom
	private boolean factorUsingV(VarUsage vu) {
	    if (subdom.isEntire()) return false;
	    if (subdom.isLH()
	    && vu.stat() == MIN 
	    && subdom.domain() == vu.domain())
	    	return true;
	    if (subdom.isRH()
	    && vu.stat() == MAX 
	    && subdom.domain() == vu.domain())
	    	return true;
	    return false;
	}

	// zeroExpr
	public TExpr zeroExpr() throws Xcept {
	    if (! (expr instanceof CompareExpr)) return this;
	    CompareExpr cexpr = (CompareExpr) expr;
	    Expr zexpr = cexpr.arg(0).sub(cexpr.arg(1));
	    TExpr texpr = new TExpr(model, zexpr);
	    texpr = texpr.restrict(subdom);
	    return texpr;
	}

	// this is a single VU? if so, return 
	public VarUsage soleVU() throws Xcept {
	    if (expr instanceof Var) 
	    	return new VarUsage(subdom, (Var) expr);
	    if (! (expr instanceof VarFuncCall)) return null;
	    VarFuncCall vfc = (VarFuncCall) expr;
	    Var v = vfc.v;
	    int[] qstat = new int[v.ndim()];
	    for (int i=0; i<qstat.length; i++) {
	    	Domain x = v.domain(i);
		qstat[i] = qstat(v, x, vfc.args.expr(i));
	    }	    
	    return new VarUsage(model, v, qstat);
	}

	// divide by expr
	protected TExpr div(Expr d) throws Xcept {
	    TExpr r = new TExpr(model, expr.div(d));
	    return r.restrict(subdom);
	}

	// simple query
	public String toString() {
	    String s = "";
	    if (!subdom.isEntire())  
	         s = "when (" + subdom.toString() + ") ";
	    return s + expr;
	}
	public TModel model() { return model; }
	public Expr expr() { return expr; }
	public TSubDom subdom() { return subdom; }
	public VarUsages usages() { return usages; }
	public boolean isEquals() {
	    return (expr instanceof IExpr) && 
		(((IExpr) expr).op() == IExpr.EQ);
	}
	public ArrayList<TExpr> arrayList() {
	    ArrayList<TExpr> list = new ArrayList<TExpr>();
	    list.add(this);
	    return list;
	}

	// Var domain substitution map
	public class Map extends Hashtable<Domain, Expr> {
	    private boolean solvable;
	    
	    protected Map() { 
	    	super();
		solvable = true;
	    }
	    protected Map(Map map) { 
	    	super(map);
		solvable = map.solvable;
	    }
	    protected void setUnsolvable() {
	    	solvable = false;
	    }
	    protected boolean isSolvable() { return solvable; }
	}
}
