/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Template for Component creation

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.PrintStream;
import java.util.ArrayList;

abstract class Templ implements Named, DiagInfo {
	protected Model model;	// attach to this model
	protected String name;	// name for template

	// constructor
	public Templ(Model m, String n) throws Xcept {
	    name = n;
	    model = m;
	    m.registerTempl(this);
	}

	// create new component from this template
	abstract public Comp createComp(Comp p, String n, Expr.List args) 
	throws Xcept;

	// methods for import
	abstract public boolean isCompatible(Templ t) throws Xcept;
	abstract public Templ clone(Model m) throws Xcept;

	// get info
	public String name() { return name; }
	public String diagInfo() { return "Template " + name; }
	abstract public Class clss();

	// dump
	public String toString() { return name; }
	abstract public void dump(PrintStream out, String indent)
	throws Xcept;

	// Templ.NList
	public static class NList extends NamedList {
            public NList(int i) { super(i); }
            public Templ templ(int i) { return (Templ) get(i); }
            public Templ templ(String n) { return (Templ) getByName(n); }
        }

}

