/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// scientific units

package JSim.expr;
import JSim.util.*;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.io.*;

public class Unit implements Named {

	// --------------------------------- STATIC VAR & METHODS
	private static Unit uscalar;
	public final static String dimless = "dimensionless";
	public static Unit scalar() {
	    if (uscalar == null) uscalar = new Unit("1");
	    return uscalar;
	}

	// bogus should never actually be used,  just tag
	private static Unit ubogus;
	static Unit bogus() throws Xcept {
	    if (ubogus == null) ubogus = new Unit("1");
	    return ubogus;
	}

	// units compatible
	static public boolean compatible(Unit a, Unit b) {
	    if (a == null || b == null) return true;
	    int ct = (a.dim.length > b.dim.length) ?
		a.dim.length : b.dim.length;
	    for (int i=0; i<ct; i++) {
		double ad = (i<a.dim.length) ? a.dim[i] : 0;
		double bd = (i<b.dim.length) ? b.dim[i] : 0;
	        if (!Util.nearlyZero(ad-bd))
		    return false;
	    }
	    return true;
	}

	// units same
	static public boolean same(Unit a, Unit b) {
	    if (a == null || b == null) return true;
	    if (!Util.nearlyZero((a.f-b.f)/a.f)) return false;
	    return compatible(a,b);
	}

	// conversion factor from b to a
	static public double convertFactor(Unit a, Unit b) {
	    if (a == null || b == null) return 1;
	    return b.f / a.f;
	}

	// return unit: s = number unitexpr
	public static Unit parse(NameSpace ns, String s) throws Xcept {
	    String sbuf = "(" + s + ")";
	    StringReader inp = new StringReader(sbuf);
	    ExprScanner scan = new ExprScanner(inp);
	    ExprParser pars = new ExprParser(scan);
	    pars.ns = ns;
	    java_cup.runtime.Symbol result = null; 
	    try {
		result = pars.parse();
	    } catch (Xcept e) {
		throw new Xcept(e.cleanMessage() + " at token <" + 
		scan.tokText() + ">");
	    } catch (Exception e) {
		throw new Xcept(e.toString() + " at token <" + 
		scan.tokText() + ">");
	    }
	    Expr expr = (Expr) result.value;
	    Unit eunit = expr.unit();
	    Unit u = new Unit(eunit.name, 
		expr.constRealVal() * eunit.f, eunit.dim);
	    return u;
	}

	// -------------------------------- INSTANCE VAR & METHODS

	public String name;	// external name of unit
	public double	f;	// conversion factor
	public double[] dim;	// mass unit dimension

	// constructors
	public Unit(String n, double ff, double[] d) 
	throws Xcept {
	    name = n;
	    f = ff;
	    if (f <= 0) throw new Xcept(
		"Zero or negative unit factor is invalid");
	    dim = (double[]) d.clone();
	}

	// renamed unit
	public Unit(String n, Unit u) {
	    this(n, u, 1.0);
	}

	// renamed unit w/ multiplier
	public Unit(String n, Unit u, double mult) {
	    name = n;
	    f = u.f * mult;
	    dim = (double[]) u.dim.clone();
	}

	// dimensionless numeric unit
	public Unit(String s)  {
	    name = s;
	    f = Util.toDouble(s);
	    dim = new double[0];
	}

	// dimensionless numeric unit
	public Unit(double d) {
	    name = "numeric";
	    f = d;
	    dim = new double[0];
	}

	// derivative unit
	public Unit deriv(Unit t) throws Xcept {
	    if (t == null) return this;
	    return binop(t, IExpr.DIV);
	}

	// binary operation on unit strings
	public Unit binop(Unit b, int op) throws Xcept {
	    Unit a = this;

	    // name and factor
	    String nname = null;
	    double nf = 0;
	    switch(op) {
	    case IExpr.MULT:
		if (same(a, scalar())) nname = b.name;
		else if (same(b, scalar())) nname = a.name;
		else nname = a.name + "*" + b.name;
		nf = a.f * b.f;
		break;
	    case IExpr.DIV:
	    case IExpr.DERIV:
	    	if (same(b, scalar())) 
		    nname = a.name;
		else {
	    	    String aname = (same(a, scalar())) ? "1" : a.name; 
		    boolean paren = b.name.indexOf('*') > 0
			|| b.name.indexOf('/') > 0;
		    nname = paren ?
			aname + "/(" + b.name + ")" :
			aname + "/" + b.name;
		}
		nf = a.f / b.f;
		break;
	    }

	    // dimensions
	    int ct = (a.dim.length > b.dim.length) ?
		a.dim.length : b.dim.length;
	    double[] ndim = new double[ct];
	    for (int i=0; i<ct; i++) {
		double ad = (i<a.dim.length) ? a.dim[i] : 0;
		double bd = (i<b.dim.length) ? b.dim[i] : 0;
		ndim[i] = (op==IExpr.MULT) ? ad + bd : ad - bd;
	    }

	    // all done
	    return new Unit(nname, nf, ndim);
	}
		
	// quick access to binops
	public Unit mult(Unit b) throws Xcept {
	    return binop(b, IExpr.MULT);
	}
	public Unit div(Unit b) throws Xcept {
	    return binop(b, IExpr.DIV);
	}

	// binary operation on unit strings
	public Unit power(Double d) throws Xcept {
	    return power(d.doubleValue());
	}
	public Unit power(double d) throws Xcept {
	    String nname = name;
	    if (! Util.nearlyZero(d-1)) {
		boolean paren = name.indexOf('*') > 0
		    || name.indexOf('/') > 0
		    || name.indexOf('^') > 0;
		nname = paren ?
		    ( "(" + name + ")^" + Util.pretty(d) )
		    : (name + "^" + Util.pretty(d));
	    }
	    double nf = Math.exp(d*Math.log(f));
	    double[] ndim = new double[dim.length];
	    for (int i=0; i<dim.length; i++) 
		ndim[i] = (dim[i] == 0) ? 0 : dim[i] * d;
		// above in case of NaN
	    return new Unit(nname, nf, ndim);
	}
		
/* causes weird bug with SBML substance/time subunit parsing
	// equality 
	public boolean equals(Object o) {
System.err.println("unit.equals():" + this + " " + o);
Thread.currentThread().dumpStack();
	    if (! (o instanceof Unit)) return false;
	    return same(this, (Unit) o);
	}
*/
	// String rep
	public final String name()  { return name; }
	public final String pubName() {
	    try {
		return PrettyUnit.name(name);
	    } catch (Xcept xcept) {
		return name;
	    }
	}
	public final String diagInfo() { return name; }
	public String toString() {
	    return toString(null);
	}
	public String toString(UnitNList ulist) {
	    return "<" + Util.pretty(f) + " " + dimString(ulist) + ">";
	}

	// fundamental dimensions string
	public String dimString(UnitNList ulist) {
	    double delta = 1e-6;
	    String s = "";
	    for (int i=0; i<dim.length; i++) {
		if (dim[i] < delta) continue;
		if (! s.equals("")) s = s + " ";
		s = s + dimString(ulist, i);
		if (Util.nearlyZero(dim[i]-1)) continue;
		s = s + "^" + Util.pretty(dim[i]);
	    }
	    if (s.equals("")) s = "1";
	    String t = "";
	    for (int i=0; i<dim.length; i++) {
		if (dim[i] > (-delta)) continue;
		if (! t.equals("")) t = t + " ";
		t = t + dimString(ulist, i);
		if (Util.nearlyZero(dim[i]+1)) continue;
		t = t + "^" + Util.pretty(-dim[i]);
	    }
	    if (!t.equals("")) t = " / " + t;
	    String d = s + t;
	    if (d.equals("1")) d = dimless;
	    return d;
	}

	// describe single unit dimension
	private String dimString(UnitNList ulist, int i) {
	    if (ulist != null && i<ulist.fund.size())
		return ulist.fund.str(i);
	    return "dim" + i;
	}

	// create parsable representation in fund units
	//   requires non-null ulist
	public String fundStr(UnitNList ulist) throws Xcept {
	    String s = Util.pretty(f) + " ";
	    int ct = 0;
	    for (int i=0; i<dim.length; i++) {
		if (Util.nearlyZero(dim[i])) continue;
		if (ct > 0) s = s + "*";
		s = s + ulist.fund.str(i) + "^";
		if (dim[i] < 0) s = s + "(";
		s = s + Util.pretty(dim[i]);
		if (dim[i] < 0) s = s + ")";
		ct++;
	    }
	    if (ct == 0) s = s + " " + dimless;
	    return s;
	}

	// add subunits to list
	public void addSubUnits(Unit.List list, UnitNList nlist) 
	throws Xcept {
	    StringTokenizer stok = new StringTokenizer(name, "()/*^");
	    int ct = stok.countTokens();
	    for (int i=0; i<ct; i++) {
		String s = stok.nextToken();
		if (! Character.isLetter(s.charAt(0))) continue;
		Unit u = nlist.byName(s);
		list.addUniq(u);
	    }
	}

	// Unit.List
	public static class List extends ArrayList<Unit> {
	    public List(int n) { super(n); }
	    public Unit unit(int i) { return (Unit) get(i); }
	    public void addUniq(Unit u) {
		if (!contains(u)) this.add(u);
	    }
	    public String toString() {
		String s = "(";
		for (int i=0; i<size(); i++) {
		    if (i>0) s = s + ",";
		    s = s + unit(i).name();
		}
		return s + ")";
	    }
	    public void writeFlat(PrintStream out, UnitNList ulist)
	    throws Xcept {
		for (int i=0; i<size(); i++) {
		    Unit u = unit(i);
		    out.println("unit " + u.name() + " = " + 
			u.fundStr(ulist) + ";");
		}
	    }
	}

	// serializable Unit info
	public static class Info implements Serializable {
	    public String name;
	    public double f;
	    public double[] dim;
	
	    // constructors
	    public Info() { }
	    public Info(Unit u) {
		name = u.name;
		f = u.f;
		dim = u.dim;
	    }

	    // create Unit
	    public Unit unit() throws Xcept {
		return new Unit(name, f, dim);
	    }
	}

}


