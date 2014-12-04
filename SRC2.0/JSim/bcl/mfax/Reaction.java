/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// general Chemical reaction


package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import java.util.ArrayList;

abstract public class Reaction extends MFComp {
	Region region;
	ChemEqn eqn;

	// constructor
	public Reaction(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    checkNParm(e,2);
	    region = (Region) getParm(e,0,Region.class);
	    if (! StringConst.class.isInstance(e.expr(1)))
		throw new Xcept(e.expr(1), "Equation string required here");
	    String s = ((StringConst) e.expr(1)).constStringVal();
	    eqn = new ChemEqn(sys, s);
	    Util.verbose("\tReaction parsed: " + eqn.diagStr());
	    region.attach(this);
	}

	// query
	public abstract boolean isFast();

	// Reaction.List
        public static class List extends Comp.List {
            public List(int n) { super(n); }
	    public Reaction reac(int n) { return (Reaction) get(n); }
        }
	// compatible unit
	public String compatibleUnit() { return Unit.dimless; }
}
