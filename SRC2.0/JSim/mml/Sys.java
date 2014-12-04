/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// system

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

abstract public class Sys extends Comp {
	public CompContext ctxt; // JSIM context relative to this component

	// constructors
	public Sys(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    ctxt = new CompContext(JSLang.lang, this);
	}

	public Sys getSys() { return this; }

	// MathSys may be flat,  all other Sys are not
	public void setFlat() throws Xcept { 
	    throw new Xcept(this, "System is not flattenable");
	}
	public boolean isFlattenable() { return false; }

	// assign units to sub-components
	public void assignUnits() throws Xcept {
	    // placeholder for now
	}
}

