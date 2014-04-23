/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test unit parser

package JSim.tests; import JSim.util.*;
import JSim.expr.*;

public class UnitTest implements NameSpace {

	// mainline
	public static void main(String[] args) throws Xcept {
	    UnitTest ns = new UnitTest();
	    for (int i=0; i<args.length; i++) {
	    	try {
	            Unit u = Unit.parse(ns, "(1 " + args[i] + ")");
		    System.out.println(args[i] + " => " + u.name() + " = " + u);
	    	} catch (Exception e) {
	    	    System.out.println("========" + args[i]);
		    e.printStackTrace();
	    	}
	    }
	}

	//// INSTANCE INFO
	public UnitNList units;

	// constructor
	public UnitTest() throws Xcept {
	    units = new UnitNList();
	    units.addFund("a");
	    units.addFund("b");
	    units.addFund("c");
	    units.addFund("d");
	    units.addFund("e");
	    units.addFund("f");
	    units.addFund("g");
	    units.add(Unit.dimless, Unit.scalar());
	}

	// get variable expression by name 
	public Expr compByName(String name) throws Xcept {
	    throw new Xcept("compByName not implemented");
	}

	// make deriv expression
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    throw new Xcept("makeDeriv not implemented");
	}

	// get unit by name
	public Unit unitByName(String name) throws Xcept {
	    return units.byName(name);
	}

	// function call
	public Expr funcCall(String name, Expr.List elist) throws Xcept {
	    throw new Xcept("funcCall not implemented");
	}
}


