/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.expr;
import JSim.util.*;

import java.io.*;

public class UnitNList extends NamedList {
	public StringList fund;	// fundamental units
	public PrefixNList prefixes; // prefixes
	public StringList prefixable; // prefixable units

	// constructors
	public UnitNList() throws Xcept {
	    super(16);	
	    fund = new StringList(8);
	    prefixes = new PrefixNList(8);
	    prefixable = new StringList(8);
	    add(Unit.dimless, Unit.scalar());
	    add("radian", Unit.scalar());
	}

	// copy
	public UnitNList copy() throws Xcept {
	    Info info = new Info(this);
	    return info.unitList();
	}

	// add fundamental unit, if not already present
	public void addFund(String s) throws Xcept {
	    if (fund.indexOf(s) >= 0) return;
	    fund.add(s);
	    int ct = fund.size();
	    double[] dim = new double[ct];
	    dim[ct-1] = 1;
	    Unit u = new Unit(s, 1, dim);
	    add(s, u);
	}

	// add unit to table under some name	
	public void add(Unit u) throws Xcept { add(u.name, u); }
	public void add(String s, Unit u) throws Xcept {
	    Unit u1 = (Unit) getByName(s);
	    if (u1 != null) {
		if (!Unit.same(u, u1)) throw new Xcept(
		    "Conflicting definitions for unit: " + s);
		return;
	    }
	    Unit u2 = new Unit(s, u.f, u.dim);
	    super.add(u2);  // superclass add
	}		

	// add prefix
	public void addPrefix(String name, double mult) throws Xcept {
	    Prefix p = new Prefix(name, mult);
	    prefixes.add(p);
	}

	// set unit prefixable
	public void setPrefixable(String name) throws Xcept {
	    if (getByName(name) == null) throw new Xcept(
		"Unit \"" + name + "\" not defined");
	    prefixable.add(name);
	}

	// query
	public Unit unit(int i) { return (Unit) get(i); }
	public String fund(int i) { return fund.str(i); }
	public Unit byName(String n) throws Xcept {
	    Unit u = (Unit) getByName(n);
	    if (u != null) return u;
 	    Prefix pfx = prefixes.prefixForName(n);
	    if (pfx == null)
	    	throw new Xcept("Unknown unit: " + n);
	    String base = n.substring(pfx.name().length());
	    if (! isPrefixable(base)) 
	    	throw new Xcept("Unknown unit: " + n);
	    u = (Unit) getByName(base);
	    if (u == null)
	    	throw new Xcept("Unknown unit: " + n);
	    u = new Unit(n, u.f*pfx.mult(), u.dim);
	    return u;
	}
	public boolean defined(String n) {
	    try {
	    	byName(n);
		return true;
	    } catch (Xcept e) {
	    	return false;
	    }
	}
	public boolean isPrefixable(String n) {
	    return prefixable.containSame(n);
	}
	public String getPrefix(double f) {
	    for (int i=0; i<prefixes.size(); i++) {
	    	Prefix pfx = prefixes.prefix(i);
		double pmult = pfx.mult();
		double z = (f-pmult)/pmult;
		if (Util.nearlyZero(z)) return pfx.name();
	    }
	    return null;
	}

	// merge unit table
	public void merge(UnitNList m) throws Xcept {

	    // fundamental translation table
	    int[] xlate = new int[m.fund.size()];
	    for (int i=0; i<m.fund.size(); i++) {
		String s = m.fund.str(i);
		addFund(s);
		xlate[i] = fund.indexOf(s);
	    }

	    // translate into new dims,  and import
	    for (int i=0; i<m.size(); i++) { 
		Unit u = (Unit) m.get(i);
		double[] ndim = new double[fund.size()];
		for (int j=0; j<u.dim.length; j++) 
		    ndim[xlate[j]] = u.dim[j];
		Unit nu = new Unit(u.name, u.f, ndim);
		add(nu.name, nu);
	    }

	    // import prefixes
	    for (int i=0; i<m.prefixes.size(); i++) {
		Prefix mpfx = m.prefixes.prefix(i);
		Prefix pfx = prefixes.prefix(mpfx.name());
		if (pfx != null && pfx.mult() != mpfx.mult())
		    throw new Xcept(mpfx,
			"Inconsistent multiple definitions for unit prefix");
		prefixes.add(mpfx);
	    }

	    // import prefixables
	    for (int i=0; i<m.prefixable.size(); i++) {
		String n = m.prefixable.str(i);
		prefixable.addUniq(n);
	    }
	}		    

	// write flattened version
	public void writeFlat(PrintStream out) throws Xcept {
	    for (int i=0; i<fund.size(); i++) 
		out.println("unit " + fund.str(i) + 
		    " = fundamental;");
	    out.println("");
	    for (int i=0; i<size(); i++) {
		Unit u = unit(i);
		out.println("unit " + u.name() + " = " + 
		    u.fundStr(this) + ";");
	    }
	}

	// dump contents
	public void dump(PrintStream out) {
	    for (int i=0; i<size(); i++) {
		Unit u = (Unit) get(i);
		out.println("\t" +
		    u.name() + "\t" + u.toString(this));
	    }
	}	

	// unit prefix
	public static class Prefix implements Named {
	    private String name;
	    private double mult; 
	    public Prefix(String n, double m) {
		name = n;
		mult = m;
	    }
	    public String name() { return name; }
	    public double mult() { return mult; }
	    public String diagInfo() {
		return "Unit Prefix " + name + "=" + mult;
	    }
	}

	// unit prefix table
	public static class PrefixNList extends NamedList {
	    public PrefixNList(int n) { super(n); }
	    public Prefix prefix(int i) { return (Prefix) get(i); }
	    public Prefix prefix(String name) {
		return (Prefix) getByName(name); 
	    }

	    // return applicable prefix, or none
	    public Prefix prefixForName(String n) {
		for (int i=0; i<size(); i++) {
		    Prefix pfx = prefix(i);
		    if (n.startsWith(pfx.name())) return pfx;
		}
		return null;
	    }
	}

	// serializable UnitNList
	public static class Info implements Serializable {
	    public String[] fund; // fundamental units
	    public Unit.Info[] units; // derived units
	    public String[] prefixable;
	    public String[] prefixNames;
	    public double[] prefixMults;

	    // constructors
	    public Info() { }
	    public Info(UnitNList list) {
		if (list == null) return;
		fund = list.fund.array();
		units = new Unit.Info[list.size()];
		for (int i=0; i<list.size(); i++) 
		    units[i] = new Unit.Info(list.unit(i));
		prefixable = list.prefixable.array();
		prefixNames = new String[list.prefixes.size()];
		prefixMults = new double[list.prefixes.size()];
		for (int i=0; i<list.prefixes.size(); i++) {
		    prefixNames[i] = list.prefixes.prefix(i).name();
		    prefixMults[i] = list.prefixes.prefix(i).mult();
		}
	    }

	    // create UnitNList from this info
	    public UnitNList unitList() throws Xcept {
		if (fund == null || units == null) 
		    return null;
		UnitNList list = new UnitNList();
		for (int i=0; i<fund.length; i++)
		    list.addFund(fund[i]);
		for (int i=0; i<units.length; i++) 
		    list.add(units[i].unit());
		for (int i=0; i<prefixNames.length; i++) 
		    list.addPrefix(prefixNames[i], prefixMults[i]);
		for (int i=0; i<prefixable.length; i++) 
		    list.setPrefixable(prefixable[i]);
		return list;
	    }
	}
}

