/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// name prettifier for units with duplicate sub-units

package JSim.expr;
import JSim.util.*;

import java.io.*;

public class PrettyUnit extends NamedList {
	public boolean hadDup; // has duplicate factor?

	// single factor constructor
	public PrettyUnit(String n) {
	    super(1);
	    hadDup = false; 
	    if (n.equals("1")) return;
	    addFactor(n, 1);
	}

	// multiply/divide constructor
	public PrettyUnit(PrettyUnit a, PrettyUnit b, boolean mult) {
	    super(a.size() + b.size());
	    hadDup = a.hadDup || b.hadDup;
	    for (int i=0; i<a.size(); i++) {
		Factor f = a.factor(i);
		addFactor(f.name, f.power);
	    }
	    for (int i=0; i<b.size(); i++) {
		Factor f = b.factor(i);
		double p = mult ? f.power : -f.power;
		addFactor(f.name, p);
	    }
	}	    

	// power constructor
	public PrettyUnit(PrettyUnit a, double p) {
	    hadDup = a.hadDup;
	    for (int i=0; i<a.size(); i++) {
		Factor f = a.factor(i);
		addFactor(f.name, f.power*p);
	    }
	}

	// add a factor
	public void addFactor(String n, double p) {
	    Factor f = factor(n);
	    if (f == null) {
		f = new Factor(n);
		add(f);
	    } else {
		hadDup = true;
	    }
	    f.power += p;
	}

	// query
	public Factor factor(int i) { return (Factor) get(i); }
	public Factor factor(String n) { return (Factor) getByName(n); }

	// format new name
	public String name() {
	    String num = null;
	    String den = null;
	    int denct = 0;
	    for (int i=0; i<size(); i++) {
	    	Factor f = factor(i);
		double p = Math.abs(f.power);
	    	if (Util.nearlyZero(p)) continue;
	        String s = f.name;
		if (! Util.nearlyZero(p-1))
		    s = s + "^" + Util.pretty(p);
		if (f.power > 0) {
		    num = (num == null) ? s : (num + "*" + s);
		} else {
		    den = (den == null) ? s : (den + "*" + s);
		    denct++;
		}
	    }
	    if (num == null) num = "1";
	    if (den == null) return num;
	    if (denct > 1) den = "(" + den + ")";
	    return num + "/" + den;
	}

	// dump for debug
	public void dump(PrintStream out) {
	    for (int i=0; i<size(); i++) {
		Factor f = factor(i);
		out.println("name=" + f.name + " power=" + f.power);
	    }
	    out.println("hadDup=" + hadDup);
	} 

	// Factor subclass
	public static class Factor implements Named {
	    public String name;
	    public double power;

	    // constructor
	    public Factor(String n) {
		name = n;
	    }

	    // query
	    public String name() { return name; }
	    public String diagInfo() { 
		return "PrettyUnit factor " + name; 
	    }
	}

	// parse Pretty unit
	private static PrettyUnit parse(String s) throws Xcept {
	    StringReader inp = new StringReader(s);
	    PrettyUnitScanner scan = new PrettyUnitScanner(inp);
	    PrettyUnitParser pars = new PrettyUnitParser(scan);
	    java_cup.runtime.Symbol result = null;
	    try {
		return (PrettyUnit) pars.parse().value;
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	}
	
	// return prettier name,  if appropriate
	public static String name(String s) throws Xcept {
	    if (s.equals("1")) return Unit.dimless;
	    PrettyUnit pu = parse(s);
	    if (s.charAt(0) == '(' || pu.hadDup) s = pu.name();
	    if (s.equals("1")) return Unit.dimless;
	    return s;
	}

	// return unit with prettied-up name, if necessary
	public static Unit unit(Unit u) throws Xcept {
	    String nname = name(u.name());
	    if (nname.equals(u.name())) return u;
	    return new Unit(nname, u);
	}

	// test program
	public static void main(String[] args) {
	    for (int i=0; i<args.length; i++) {
		String s = args[i];
		System.out.println("=========" + s);
		try {   
		    PrettyUnit pu = parse(s);
	    	    pu.dump(System.out);
	    	    System.out.println("  => " + pu.name());
		} catch (Xcept e) {
		    e.printStackTrace();
		}
	    }
	}

}

