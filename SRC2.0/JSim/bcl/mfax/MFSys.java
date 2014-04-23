/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MFAX system

package JSim.bcl.mfax;

import java.io.PrintStream;
import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class MFSys extends Sys {
	Time t;
	Chem.List chem;
	Expr.List tlist;

	// constructor
	public MFSys(Comp p, String n, Expr.List e) throws Xcept {
            super(p, n, e);
	    chem = new Chem.List(4);
	    tlist = null;
        }

	// t-only Expr list,  for variable construction
	public Expr.List tlist() throws Xcept {
	    if (tlist == null) {
	    	tlist = new Expr.List(1);
	    	tlist.add(t);
	    }
	    return tlist;
	}

	// liters unit
	public Unit liter() throws Xcept {
	    return getModel().parseUnit("liter");
	}

	// simplify unit, if defined in unit table
	public Unit simplify(Unit u) throws Xcept { 
	    UnitNList units = getModel().units;
	    for (int i=0; i<units.size(); i++) {
		Unit u1 = units.unit(i);
		if (! Unit.same(u, u1)) continue;
		// System.err.println("  subbed " + 
		    // u.pubName() + " -> " + u1.pubName());
		return u1;
	    }
	    return u;
	}

	// assign component units
	public void assignUnits() throws Xcept {

	    // unit processing?
	    Model model = getModel();
	    if (model.unitControl == Model.OFF) 
		throw new Xcept(this,
		   "MFAX system does not support <unit conversion off>");

	    // unit processing!
	    if (model.unitControl == Model.ON) {

		// check Time units
		Unit tunit = model.parseUnit("second");
		if (t.unit() == null || !Unit.compatible(t.unit(), tunit)) throw new Xcept(t,
		    "Time component unit must be compatible with seconds");

		// check Chem units
		for (int i=0; i<chem.size(); i++) {
		    Comp c = chem.comp(i);
		    ((MFComp) c).assignUnits();
		}

		// check/set other component units
		for (int i=0; i<child.size(); i++) {
		    Comp c = child.comp(i);
		    if (MFComp.class.isInstance(c))
			((MFComp) c).assignUnits();
		}
	    }
	}

	// write Flat
	public void writeFlat(PrintStream out, Context ctxt) throws Xcept {

	    // expand child derivatives required before processing
	    expandChildDerivs();

	    // solve flow sub-components
	    for (int i=0; i<child.size(); i++) {
		Comp c = (Comp) child.get(i);
		if (c.isVar()) continue;
		if (! MFComp.class.isInstance(c)) throw new Xcept(c,
		    "Illegal MFAX component");
		((MFComp) c).solve1();
	    }	

	    // solve conc sub-components
	    for (int i=0; i<child.size(); i++) {
		Comp c = (Comp) child.get(i);
		if (c.isVar()) continue;
		((MFComp) c).solve2();
	    }	

	    // write MathSys
	    out.println("math " + name() + " {");
	    super.writeFlat(out, ctxt);
	    out.println("}");	// terminate MathSys
	}
}
