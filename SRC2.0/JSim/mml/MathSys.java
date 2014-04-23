/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// default math system

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*;

import java.io.PrintStream;
import java.util.ArrayList;

public class MathSys extends Sys {

	// 1.6 compiler
	protected Var.List flatVars;
	protected boolean isFlat;

	// 1.7 compiler
	protected ArrayList<Var> vars; // flat vars
	protected ArrayList<Domain> domains; // flat domains
	
	// constructors
	public MathSys(Comp p, String n, Expr.List e) throws Xcept {
            super(p, n, e);
	    vars = new ArrayList<Var>();
	    domains = new ArrayList<Domain>();
        }

	// register child (subclasses may restrict components)
	public void registerChild(Comp c) throws Xcept {
	    super.registerChild(c);
	    if (! isFlat) return;
	    if (! Var.class.isInstance(c)) throw new Xcept(c,
		"Non-variable created within flat MathSys");
	    setFlat((Var) c);
	}

	// set 1 var flatInx
	public void setFlat(Var v) throws Xcept {
	    if (v.flatInx != 0) throw new Xcept(v, "duplicate setFlat index");
	    v.flatInx = flatVars.size();
	    flatVars.add(v);
	}

	// if only Vars,  system is flattenable
	public boolean isFlattenable() {
	    for (int i=0; i<child.size(); i++) {
		Comp c = child.comp(i);
		if (! Var.class.isInstance(c)) 
		    return false;
	    }
	    return true;
	}   

	// create flat inx of vars
	public void setFlat() throws Xcept {
	    if (isFlat) return;
	    flatVars = new Var.List(child.size());
	    for (int i=0; i<child.size(); i++) {
		Comp c = child.comp(i);
		if (! Var.class.isInstance(c)) return;
		setFlat((Var) c);
		if (! Domain.class.isInstance(c)) continue;
		Domain x = (Domain) c;
		setFlat(x.vmin);
		setFlat(x.vmax);
		setFlat(x.vct);
		setFlat(x.vdelta);
	    }
	    for (int i=0; i<eqn.size(); i++) {
		Eqn e = eqn.eqn(i);
		e.flatInx = i;
	    }
	    isFlat = true;
	    return;
	}

	// write flatmath 
	public void writeFlat(PrintStream out, Context ctxt) 
	throws Xcept {
	    boolean isTop = (parent.parent == null); // ??? HACK
	    if (isTop) out.println("math " + name + " {");
	    super.writeFlat(out, ctxt);
	    if (isTop) out.println("}");
	}

	// get em'
	public int nVar() {
	    if (! isFlat) throw new Error(
		"MathSys is not flat (nVar)");
	    return flatVars.size();
	}
	public Var var(int i) throws Error {
	    if (! isFlat) throw new Error(
		"MathSys is not flat (var)");
	    return (Var) flatVars.comp(i);
	}
	public boolean isFlat() { return isFlat; }
}
