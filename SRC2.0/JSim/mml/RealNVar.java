/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// N-dimensional dependent real variable

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

public class RealNVar extends NVar {
	private RealNVar underiv;
	private Domain derivdomain;		
	
	// constructor
	public RealNVar(Comp p, String n, Expr.List d) 
	throws Xcept {
	    super(p, n, d);
	}

	// create derivative
	public Var createDeriv(Domain t) throws Xcept {
	    if (!hasDomain(t)) throw new Xcept(this,
		"variable has no derivate wrt " + t.name);
	    String n = name + ":" + t.name;
	    RealNVar v = new RealNVar(parent, n, args);
	    v.underiv = this;
	    if (unit != null) {
		Unit u2 = (t.unit == null) ? 
		    unit : PrettyUnit.unit(unit.deriv(t.unit));
		v.setUnit(u2);
	    }	
	    v.derivdomain = t;
	    if (isPrivate()) v.setAccess(PRIVATE);
	    Util.verbose("Creating deriv variable " + v.toString());
	    return v;
	}

	// derivative stuff
	public Var unDeriv() throws Xcept {
	    if (underiv == null) throw new Xcept(this,
		"no unDeriv() for this var");
	    return underiv;
	}
	public Domain derivDomain() throws Xcept {
	    if (derivdomain == null) throw new Xcept(this,
		"no derivDomain() for this var");
	    return derivdomain;
	}
}

