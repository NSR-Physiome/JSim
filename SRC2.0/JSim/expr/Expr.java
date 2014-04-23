/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// expression

package JSim.expr;
import JSim.util.*;

import java.util.ArrayList;
import java.io.StringReader;

public abstract class Expr implements DiagInfo {

	/////////////// STATIC VARS & METHODS

	// data types
	public static final int UNDEFINED = 0;
	public static final int VOID = 1;
	public static final int BOOLEAN = 2;
//	public static final int INTEGER = 3;
	public static final int REAL = 4;
	public static final int STRING = 8;

	// datatype name
	public static String dataTypeName(int i) {
	    switch (i) {
	    case UNDEFINED: return "UNDEFINED";
	    case VOID: return "VOID";
	    case BOOLEAN: return "BOOLEAN";
//	    case INTEGER: return "INTEGER";
	    case REAL: return "REAL";
	    case STRING: return "STRING";
	    default: return "UNKNOWN";
	    }
	}

	public static final ConstExpr zero = new RealConst(0);
	public static final ConstExpr one = new RealConst(1);
	public static final ConstExpr negone = new RealConst(-1);
	public static final ConstExpr pi = new RealConst(Math.PI);
	public static final ConstExpr exponentiale = new RealConst(Math.E);
	public static final ConstExpr truex = new BoolConst(true);
	public static final ConstExpr falsex = new BoolConst(false);

	public static ConstExpr cons(double d) {
	    if (d == 0) return zero;
	    if (d == 1) return one;
	    if (d == -1) return negone;
	    return new RealConst(d);
	}
	public static ConstExpr cons(boolean b) {
	    return b ? truex : falsex;
	}

	public static Expr parse(NameSpace ns, String s) throws Xcept {
	    StringReader inp = new StringReader(s);
	    ExprScanner scan = new ExprScanner(inp);
	    ExprParser pars = new ExprParser(scan);
	    pars.ns = ns;
	    java_cup.runtime.Symbol result = null; 
	    try {
		result = pars.parse();
	    } catch (Exception e) {
		String msg = (e instanceof Xcept) ?
		    ((Xcept) e).cleanMessage() : e.toString();
		if (! Util.isBlank(scan.tokText()))
		    msg = msg + " at token <" + scan.tokText() + ">";
		msg = msg + " while parsing expression " + s;
		throw new Xcept(msg);
	    }
	    Expr expr = (Expr) result.value;
	    return expr;
	}

	// minimum 
	public static Expr min(Expr e1, Expr e2) throws Xcept {
	    Expr e0 = new CompareExpr(IExpr.LE, e1, e2);
	    return new IfExpr(e0, e1, e2);
	}

	// maximum
	public static Expr max(Expr e1, Expr e2) throws Xcept {
	    Expr e0 = new CompareExpr(IExpr.GE, e1, e2);
	    return new IfExpr(e0, e1, e2);
	}

	/////////////// DYNAMIC VARS & METHODS	

	// constructor
	public Expr() {	
	    // nothing now
	}

	// query methods
	abstract public int dataType(); // BOOLEAN, REAL etc
	public NamedExpr getNamed() { return null; }
	public boolean isDomain() { return false; }
	abstract public boolean sameAs(Expr e);
	public double realVal(Context ctxt) throws Xcept { 
	    throw new Xcept("Real-valued expression expected"); 
	}
	public boolean boolVal(Context ctxt) throws Xcept { 
	    throw new Xcept("Boolean expression expected"); 
	}
	public String stringVal(Context ctxt) throws Xcept { 
	    throw new Xcept("String-valued expression expected");
	}
	public double constRealVal() throws Xcept {
	    return realVal(ConstContext.jsim);
	}
	public boolean constBoolVal() throws Xcept { 
	    return boolVal(ConstContext.jsim);
	}
	public String constStringVal() throws Xcept { 
	    return stringVal(ConstContext.jsim);
	}
	abstract public String toString(Context ctxt);
	public boolean needsParen(int argInx, int binop) { return false; }
	abstract public void addNamedExpr(Expr.List list) throws Xcept;
	abstract public void addDomains(Expr.List list);

	public String diagInfo() {
	    return "Expression \"" + toString() + "\"";
	}
	
	// get unit,  may be null
	abstract public Unit unit();

	// return unit-corrected version of this expr
	abstract public Expr unitCorrect() throws Xcept;

	// replace sub-exprs in list1 with those in list2
	public Expr replace(Expr.List list1, Expr.List list2)
	throws Xcept {
	    if (list1.size() != list2.size()) throw new Xcept(this,
		"Expr.replace() called with mismatching lists");
	    for (int i=0; i<list1.size(); i++) 
		if (list1.expr(i).sameAs(this))
		    return list2.expr(i);
	    return this;
	}

	// shortcut for 4 function operations
	public Expr eq(Expr e1) throws Xcept {
	    return new CompareExpr(IExpr.EQ, this, e1);
	}
	public Expr add(Expr e1) throws Xcept {
	    return new RealBExpr(IExpr.ADD, this, e1);
	}
	public Expr sub(Expr e1) throws Xcept {
	    return new RealBExpr(IExpr.SUB, this, e1);
	}
	public Expr mult(Expr e1) throws Xcept {
	    return new RealBExpr(IExpr.MULT, this, e1);
	}
	public Expr div(Expr e1) throws Xcept {
	    return new RealBExpr(IExpr.DIV, this, e1);
	}
	public Expr pow(Expr e1) throws Xcept {
	    return new RealBExpr(IExpr.POW, this, e1);
	}
	public Expr pow(double n) throws Xcept {
	    return pow(Expr.cons(n));
	}
	public Expr pow(int n) throws Xcept {
	    return pow((double) n);
	}
	public Expr and(Expr e1) throws Xcept {
	    return new LogicalExpr(IExpr.AND, this, e1);
	}
	public Expr or(Expr e1) throws Xcept {
	    return new LogicalExpr(IExpr.OR, this, e1);
	}
	public Expr not() throws Xcept {
	    return new NotExpr(this);
	}

	// constant expression
	public boolean isConst() { return false; } 
	public ConstExpr cons() throws Xcept {
	    switch(dataType()) {
	    case BOOLEAN: return cons(constBoolVal());
	    case REAL: return cons(constRealVal());
	    }
	    throw new Xcept(this, "cons(): unsupported dataType");
	}

	// hack for exponent unit correction, fix in new expr package???
	public boolean isConstIgnoreUnits() { return isConst(); }

	// multiply for unit correction, if necessary
	public Expr multUnit(Unit u1, Unit u2) 
	throws Xcept {
	    double f = Unit.convertFactor(u1, u2);
	    if (Util.nearlyZero(f-1)) return this;
	    return mult(Expr.cons(f));
	}
	
	// take symbolic derivative 
	// expandDeriv() LATER: return this if no embedded DERIV ???
	abstract public Expr expandDeriv() throws Xcept;
	abstract public Expr takeDomDeriv(NamedExpr t) throws Xcept;
	public Expr takeDeriv(Expr e) throws Xcept {
	    if (e.isDomain()) return takeDomDeriv((NamedExpr) e);
	    Expr.List dlist = new Expr.List(1);
	    e.addDomains(dlist);
	    if (dlist.size() == 0)
	        return takeDomDeriv((NamedExpr) e);
	    	//throw new Xcept(this,
		//   "Cannot take deriv WRT invariant expression");
	    Expr eret = null;
	    for (int i=0; i<dlist.size(); i++) {
		NamedExpr x = (NamedExpr) dlist.get(i);
		Expr e1 = takeDomDeriv(x);
		Expr e2 = e.takeDomDeriv(x);
		Expr e3 = e1.div(e2);
		eret = (eret == null) ? e3 : eret.add(e3);
	    }
	    return eret;
	}

	// simplify
	abstract public Expr simplify() throws Xcept;

	// expand sum - currently unused
	public Expr.List expandSum() throws Xcept {
	    Expr.List list = new Expr.List(1);
	    list.add(this);
	    return list;
	}

	// find linear factor
	public Expr linearFactor(NamedQueryExpr q, boolean keep) throws Xcept {
	    return keep ? zero : this;
	}

	// Expr.List
	public static class List extends ArrayList<Expr> {
	    public List() { super(); }
	    public List(int n) { super(n); }
	    public List(Expr[] exprs) {
	    	this(exprs.length);
	 	for (int i=0; i<exprs.length; i++)
		    add(exprs[i]);
	    }
	    public Expr expr(int i) { return (Expr) get(i); }
	    public String toString() {
		return toString(null);
	    }
	    public void addUniq(Expr e) {
		if (!contains(e)) this.add(e);
	    }
	    public void addUniq(Expr.List elist) {
		for (int i=0; i<elist.size(); i++) {
		    Expr e = elist.expr(i);
		    if (!contains(e)) this.add(e);
		}
	    }
	    public boolean sameAs(Expr.List elist) {
		for (int i=0; i<elist.size(); i++) 
		    if (!expr(i).sameAs(elist.expr(i)))
			return false;
		return true;
	    }
	    public boolean containSet(Expr.List elist) {
		for (int i=0; i<elist.size(); i++) {
		    Expr e = elist.expr(i);
		    if (!contains(e)) return false;
		}
		return true;
	    }
	    public boolean containAny(Expr.List elist) {
		for (int i=0; i<elist.size(); i++) {
		    Expr e = elist.expr(i);
		    if (containSame(e)) return true;
		}
		return false;
	    }
	    public boolean containSame(Expr e) {
		for (int i=0; i<size(); i++) {
		    Expr e1 = expr(i);
		    if (e.sameAs(e1)) return true;
		}
		return false;
	    }
	    public boolean equalSet(Expr.List elist) {
		return (this.containSet(elist) 
		   &&  elist.containSet(this));
	    }
	    public boolean xsects(Expr.List elist) {
		for (int i=0; i<size(); i++) 
		    if (elist.contains(expr(i))) return true;
		return false;
	    }
	    public void sub(Expr e) {
		int j = indexOf(e);
		if (j >= 0) remove(j);
	    }	
	    public void subSame(Expr e) {
		for (int i=0; i<size(); i++) {
		    if (! e.sameAs(expr(i))) continue;
		    remove(i);
		    i--;
		}
	    }	
	    public void sub(Expr.List elist) {
		for (int i=0; i<elist.size(); i++) 
		    sub(elist.expr(i));
	    }
	    public String toString(Context ctxt) {
		String s = "(";
		for (int i=0; i<size(); i++) {
		    Expr e = expr(i);
		    if (i>0) s = s + ",";
		    s = s + 
			((ctxt==null) ? e.toString() : e.toString(ctxt));
		}
		return s + ")";
	    }
	    public Expr.List copy() {
		Expr.List elist = new Expr.List(size());
		for (int i=0; i<size(); i++)
		    elist.add(expr(i));
		return elist;
	    }
	    public StringList stringList() {
		StringList slist = new StringList(size());
		for (int i=0; i<size(); i++) 
		    slist.add(expr(i).toString());
		return slist;
	    }
	}

}
