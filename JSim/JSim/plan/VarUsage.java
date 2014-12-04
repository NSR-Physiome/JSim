/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// how a variable used in an expression

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class VarUsage {
	private TModel model;
	private Var v;
	private int stat; // primary state QUERY-MAX
	private Domain domain; // domain if MIN/MAX
	private int[] qstat; // QUERY stats by Var.domainList() (CURR-EXPR)
	private boolean solvable;
	private int hashCode; 
	private NamedQueryExpr qexpr;

	// stat value only
	public static final int QUERY = 0;

	// stat or qstat values
	public static final int CURR = 1;
	public static final int MIN = 2;
	public static final int MAX = 3;

	// qstat values only
	public static final int DELAY = 4;
	public static final int XEXPR = 5; // x-variant expr
	public static final int EXPR = 6;  // x-constant expt

	// qstat() for domain not in v
	public static final int ABSENT = -1;

	// constructors
	public VarUsage(TModel model, Var v) {
	    this.model = model;
	    this.v = v;
	    stat = CURR;
	    solvable = true;
	    calcHash();
	    model.vus.add(this);
	}
	public VarUsage(TSubDom sd, Var v) {
	    this.model = sd.model();
	    this.v = v;
	    domain = sd.domain();
	    stat = CURR;
	    if (domain != null && v.hasDomain(domain)) 
	        stat = sd.isLH() ? MIN : MAX;
	    solvable = true;
	    calcHash();
	    model.vus.add(this);
	}	    	
	public VarUsage(TModel model, Var v, int stat, Domain x) {
	    this.model = model;
	    this.v = v;
	    this.stat = stat;
	    domain = x;
	    solvable = true; 
	    calcHash();
	    model.vus.add(this);
	}
	public VarUsage(TModel model, Var v, int[] qstat) throws Xcept {
	    this.model = model;
	    this.v = v;
	    stat = CURR;
	    solvable = true;
	    if (qstat.length != v.ndim()) throw new Xcept(
	    	"VarUsage qstat dimensional mismatch");
	    for (int i=0; i<qstat.length; i++) {
	    	switch (qstat[i]) {
		case CURR: 
		case ABSENT:
		    break;
		case MIN:
		case MAX:
		    stat = (stat == CURR) ? qstat[i] : QUERY;
		    domain = v.domain(i);
		    break;
		default:
		    stat = QUERY;
		    break;
		}
	    }
	    if (stat == QUERY) {
	    	this.qstat = qstat;
		domain = null;
		solvable = false;
	    }
	    calcHash();
	    model.vus.add(this);
	}
	
	// calculate hashCode for effticient LinkedHashSet's  NEEDS WORK ???
	private void calcHash() {
	    hashCode = v.hashCode();
	    int mult = 10000;
	    for (int i=0; i<v.ndim(); i++) {
	    	Domain x = v.domain(i);
	    	hashCode += mult * qstat(x);
		mult *= 10;
	    }
	}

	// set unsolvable (min/max in integral e.g. not solvable)
	protected void setUnsolvable() { solvable = false; }

	// operators
	public VarUsage restrict(TSubDom sd) throws Xcept {
//System.err.println("restrict: " + this + " by " + sd);
	    if (sd.isEntire()) return this;
	    Domain x = sd.domain();
	    int xinx = indexOf(x);
	    if (xinx < 0) return this;

	    // xstat ok?
	    boolean ok = qstat(x) == CURR;
	    if (sd.isLH() && qstat(x) == DELAY) ok = true;
	    if (!ok) return this;

	    // special case for domain
	    // disabled because of when(t=t.min) procTool;
	    // maybe causes other problems?
//	    if (v instanceof Domain) {
//	    	x = (Domain) x;
//	    	Var vbc = sd.isLH() ? x.vmin : x.vmax;
//		return new VarUsage(model, vbc);
//	    }

	    // create restricted VarUsage
	    int xstat = sd.isLH() ? MIN : MAX;
	    int[] nqstat = new int[v.ndim()];
	    for (int i=0; i<nqstat.length; i++) 
	    	nqstat[i] = (i == xinx) ? xstat : qstat(v.domain(i));
	    VarUsage vu = new VarUsage(model, v, nqstat);
	    if (! solvable) vu.setUnsolvable();
//System.err.println("  restrict: " + this + " becomes " + vu);
	    return vu;
	}

	// take deriv wrt x, return null if not x variant
	protected VarUsage deriv(Domain x) throws Xcept {
	    if (! v.hasDomain(x)) return null;
	    if (stat == QUERY) return null;
	    if (domain == x) return null;
	    Var vx = v.deriv(x); // may create vx
	    model.updateVarMaps();
	    return new VarUsage(model, vx, stat, domain);
	}

	// build sequence loops
	protected DomainSet seqLoops() throws Xcept {
	    DomainSet seqLoops = new DomainSet();
	    addSeqLoops(seqLoops);
	    return seqLoops;
	}	    
	protected void addSeqLoops(DomainSet seqLoops) throws Xcept {
	    for (int i=0; i<v.ndim(); i++) {
	        Domain x = v.domain(i);
	    	int q = qstat(x);
		if (q == MIN || q == MAX || q == EXPR) continue;
		seqLoops.add(x);
	    }
	}

	// test equality
	public boolean equals(Object o) {
	    if (! (o instanceof VarUsage)) return false;
	    VarUsage vu = (VarUsage) o;
	    if (v != vu.v) return false;
	    if (stat != vu.stat) return false;
	    if (domain != vu.domain) return false;
	    if (stat == QUERY) 
	    	for (int i=0; i<qstat.length; i++) 
	    	    if (qstat[i] != vu.qstat[i]) return false;
	    return true;
	}

	// string rep for SeqNode.name
	public String nodeString() {
	    String s = v.toString();
	    switch (stat) {
	    case CURR:
	    	break;
	    case MIN:
	        if (v.isDomain()) 
		   s += "(min)";
	    	else 
		   s += "(" + domain + ".min)";
		break;
	    case MAX:
	        if (v.isDomain()) 
		   s += "(max)";
	    	else 
		   s += "(" + domain + ".max)";
		break;
	    default:
		int ct = 0;
	    	for (int i=0; i<qstat.length; i++) {
		    Domain x = v.domain(i);
		    String tok = "" + x;
		    switch (qstat[i]) {
		    case CURR: tok = null; break;
		    case MIN:  tok += ".min"; break;
		    case MAX:  tok += ".max"; break;
		    case DELAY:  tok += ".delay"; break;
		    case XEXPR:  tok += ".xexpr"; break;
		    case EXPR:  tok += ".expr"; break;
		    default: tok += ".???"; break;
		    }
		    if (tok == null) continue;
		    s += (ct == 0) ? "(" : ",";
		    s += tok;
		    ct++;
		}
		if (ct>0) s += ")";
	    }
	    return s;
	}

	// query expr
	public NamedQueryExpr qexpr() throws Xcept {
	    if (qexpr != null) return qexpr;
	    if (stat == CURR) {
	    	qexpr = v;
	    } else if (stat == MIN || stat == MAX) {
	        Expr.List args = new Expr.List();
		for (int i=0; i<v.ndim(); i++) {
		    Domain x = v.domain(i);
		    Expr arg = x;
		    if (x == domain)
		    	arg = (stat == MIN) ? x.vmin : x.vmax;
		    args.add(arg);
		}
		qexpr = new VarFuncCall(v, args);
	    } else {
	        throw new Xcept(
	    	    "VarUsage: No qexpr available for " + this);
	    }
	    return qexpr;
	}
	
	// string rep
	public String toString() {
	    String s = nodeString();
	    if (! solvable) s = s + "*";
	    return s;
	}

	// simple query
	public TModel model() { return model; }
	public Var v() { return v; }
	public int stat() { return stat; }
	public Domain domain() { return domain; }
	private int indexOf(Domain x) {
	    return v.domainList().indexOf(x); 
	}
	public int qstat(Domain x) { 
	    switch (stat) {
	    case CURR: return CURR;
	    case MIN:  return (domain == x) ? MIN : CURR;
	    case MAX:  return (domain == x) ? MAX : CURR;
	    default:  
	        int inx = indexOf(x);
		return (inx < 0) ? ABSENT : qstat[inx];
	    }
	}
	public int hashCode() { return hashCode; }
	public boolean isCurr() { return stat == CURR; }
	public boolean isCurr(Var v) { return v==this.v && stat==CURR; }
	public boolean isBoundary() { 
	    return stat == MIN || stat == MAX;
	}
	public boolean isMin() { return stat == MIN; }
	public boolean isMin(Domain x) { 
	    return stat == MIN && domain == x; } 
	public boolean isSolvable() { return solvable; } 
}
