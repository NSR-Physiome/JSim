/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Unit types (equivalence classes by dimensionality)

package JSim.expr;
import JSim.util.*;

import java.io.*;
import java.util.*;

public class UnitTypes implements NameSpace {
	private UnitNList units;
	private Hashtable<UnitDims,String> types;
	private StringList warnings;

	// constructor
	public UnitTypes(UnitNList units) throws Xcept {
	    this.units = units;
	    types = new Hashtable<UnitDims,String>();
	    warnings = new StringList();
	}

	// load types from file
	public void loadTypes(File f) throws Xcept {
	    loadTypes(UtilIO.readText(f));
	}
	
	// load types from text string
	public void loadTypes(String text) throws Xcept {
	    StringList list = new StringList(text, "\n");
	    for (int i=0; i<list.size(); i++) {
	    	String line = list.str(i);
		if (Util.isBlank(line)) continue;
		StringTokenizer stok = new StringTokenizer(line);
		String t1 = stok.nextToken();
		if (t1.startsWith("//")) continue;
		if (stok.countTokens() != 1) throw new Xcept(
		    "Bad unit types file format, line " + (i+1) 
		    + " : " + line);
		String t2 = stok.nextToken();
		loadType(t1, t2);
 	    }
	}

	// load unit type
	public void loadType(String unitexpr, String type) 
	throws Xcept {
	    Unit u = Unit.parse(this, "1 " + unitexpr);
	    UnitDims dims = new UnitDims(u);
	    String t = types.get(dims);
	    if (t != null) 
	        warning("Unit type conflict: " + t + " vs. " + type);
	    else
	    	types.put(dims, type);
	}

	// warning
	private void warning(String msg) throws Xcept {
//	    System.err.println(msg);
	    warnings.add(msg);
	}
	public StringList getWarnings() { return warnings; }

	// get unit type from unit
	public String getType(Unit u) {
	    UnitDims dims = new UnitDims(u);
	    String t = types.get(dims);
	    if (t == null) {
	    	t = u.dimString(units);
		types.put(dims, t);
	    }
	    return t;
	}

	// NameSpace methods
	public Unit unitByName(String name) throws Xcept {
	    return units.byName(name);
	}
	public Expr compByName(String name) throws Xcept {
	    throw new Xcept("compByName not implemented");
	}
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    throw new Xcept("makeDeriv not implemented");
	}
	public Expr funcCall(String name, Expr.List elist) throws Xcept {
	    throw new Xcept("funcCall not implemented");
	}

	// dim class
	public class UnitDims {
	    private Unit u;
	    private int hashCode;
	    public UnitDims(Unit u) {
	        this.u = u;
		hashCode = u.dimString(units).hashCode();
	    }
	    public int hashCode() { return hashCode; }
	    public boolean equals(Object o) {
	    	if (! (o instanceof UnitDims)) return false;
		return Unit.compatible(u, ((UnitDims) o).u);
	    }
	    public String toString() { return u.dimString(units); }
	}
	
}

