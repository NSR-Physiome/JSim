// unit name utilities for MML/SBML/CellML translators

package JSim.mml;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.io.*;
import java.util.*;

public class UnitNames {
	private UnitNList units;
	private Hashtable<String, Unit> nameMap;
	private Hashtable<Dims, Unit> dimMap;

	// constructor
	public UnitNames() throws Exception {
	    File f = new File(Util.jsimHome());
	    f = new File(f, "common");
	    f = new File(f, canonicalFile() + ".mod");
	    ModelReader rdr = new ModelReader(f, null, null);
	    units = rdr.units;
	    initMaps();
	}
	public UnitNames(UnitNList units) throws Exception {
	    this.units = units;
	    initMaps();
	}

	// initialize nameMap & dimMap 
	private void initMaps() throws Xcept {
	    nameMap = new Hashtable<String, Unit>();
	    dimMap = new Hashtable<Dims, Unit>();
	    ArrayList<Unit> ulist = new ArrayList<Unit>();
	    for (int i=0; i<units.size(); i++) {
	    	Unit u = units.unit(i);
		Dims d = new Dims(u);
		Unit u0 = dimMap.get(d);
		if (u0 == null || u.name.length() < u0.name.length())
		    dimMap.put(d, u);
	    }
	}

	// name of canonical unit file
	public static String canonicalFile() { 
	    return "nsrunit";
	}
	
	// canonical (short) name for a unit, null if no such unit
	public String canonicalName(String n) {
	    Unit u;
	    try {
	    	u = units.byName(n);
	    } catch (Xcept e) {
	    	return null;
	    }
	    if (u == null) return null;
	    Unit un = nameMap.get(n);
	    if (un != null) return un.name;
	    Dims dims = new Dims(u);
	    Unit u0 = dimMap.get(dims);
	    if (u0 == null) 
	    	nameMap.put(u.name, u);
	    else
	    	u = u0;
	    return u.name;
	}

	// unit dimension hash
	public static class Dims {
	    public Unit unit;
	    public int hashCode;
	    private static final int DK = 4; // max dim in hashCode
	    
	    public Dims(Unit u) {
	    	unit = u;
		hashCode = 0;
		int ndim = (u.dim == null) ? 0 : u.dim.length;
		for (int i=0; i<ndim; i++) {
		    String dhs = "" + u.dim[i];
		    dhs = dhs.substring(0,1);
		    int dh;
		    try { 
		    	dh = Util.toInt(dhs);
		    } catch (Xcept e) {
		    	dh = 0;
		    }
		    if (dh > DK) dh = DK;
		    if (dh < -DK) dh = -DK;
		    hashCode *= DK*2;
		    hashCode += DK + dh;
		}
		int df = (int) Math.log10(u.f);
		hashCode *= 100;
		hashCode += 50 + df;
	    }
	
	    // query
	    public int hashCode() { return hashCode; }
	    public String toString() { return unit.toString(); }
	    public String name() { return unit.name; }
	    public boolean equals(Object o) {
	    	if (! (o instanceof Dims)) return false;
		Dims dims = (Dims) o;
		return Unit.same(unit, dims.unit);
	    }		    	
	}

	// test program
	public static void main(String[] args) throws Exception {
	    UnitNames names = new UnitNames();
	    for (int i=0; i<args.length; i++) {
	    	String oldName = args[i];
		String newName = names.canonicalName(oldName);
	        System.out.println(oldName + " => " + newName);
	    }
	}

}
