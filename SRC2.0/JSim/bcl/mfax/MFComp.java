/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MFAX component

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

abstract class MFComp extends Comp {
	protected MFSys sys;	// part of this system
	protected Unit unit;	// unit for this component

	// constructor
	public MFComp(Comp p, String n, Expr.List e) throws Xcept {
            super(p, n, e);

	    // hack for template MFComps with null parent
	    if (parent == null) {
		sys = new MFSys(null, n + ".systemplate", null);
		parent = sys;
		return;
	    }

	    try {
	    	sys = (MFSys) parent;
	    } catch (ClassCastException e1) {
		throw new Xcept(parent,
		name + " parent must be MFAX.MFSys");
	    }
	}

	// add internal variables for each ChemSpecies - empty by default
	protected void addChem(Chem c) throws Xcept {
	}

	// solve phase 1 (flow)
	protected void solve1() throws Xcept {
//	     System.out.println("Solve1 " + name);
	}

	// solve phase 2 (conc)
	protected void solve2() throws Xcept {
//	     System.out.println("Solve2 " + name);
	}

	// system time unit
	protected Unit timeUnit() { return sys.t.unit(); }

	// get concentration variable
	protected RealNVar conc(Chem c) throws Xcept {
	    RealNVar v = (RealNVar) getChild(c.name());
	    if (v == null) throw new Xcept(this,
		"Internal concentration variable not found: " +
		 c.name());
	    return v;
	}

	// add new concentration variable
	protected RealNVar addConc(Chem c) throws Xcept {
	    RealNVar v = new RealNVar(this, c.name(), sys.tlist());
	    return v;
	}

	// check # params in component list
	protected void checkNParm(Expr.List e, int n)
	throws Xcept {
	    int ct = (e == null) ? 0 : e.size();
	    if (ct != n) throw new Xcept (
		getClass().getName() + " requires " + n +
		" arguments");
	}

	// get component parameter from Expression list
	protected Comp getParm(Expr.List e, int n, Class clss)
	throws Xcept {
	    Comp c = (Comp) e.expr(n).getNamed();
	    if (c == null) throw new Xcept(this,
		"Argument #" + (n+1) + " must be simple component");
	    if (!clss.isInstance(c)) throw new Xcept(this,
		"Argument #" + (n+1) + " must be of class " +
		clss.getName());
	    return (Comp) c;
	}

	// return variable set expr,  null if not set
	protected Expr varExpr(SubDom sd, Var v) throws Xcept {

	    // find any matching eqns ?
	    Eqn ematch = null;
	    for (int i=0; i<sys.eqn.size(); i++) {
		Eqn e = sys.eqn.eqn(i);
		if (e.lhs != v) continue;
		if (!sd.sameAs(e.sdom())) continue;
		ematch = e;
		break;
	    }
	    for (int i=0; i<eqn.size(); i++) {
		Eqn e = eqn.eqn(i);
		if (e.lhs != v) continue;
		if (!sd.sameAs(e.sdom())) continue;
		ematch = e;
		break;
	    }
	    return (ematch == null) ? null : ematch.rhs;
	}

	// is Variable set ?
	protected boolean varSet(SubDom sd, Var v) throws Xcept {
	    return (varExpr(sd, v) != null);
	}

	// set Variable value
	protected void setVar(Var v, Expr expr, boolean force)
	throws Xcept {
	    setVar(SubDom.entire(), v, expr, force);
	}
	protected void setVar(SubDom sd, Var v, Expr expr, boolean force)
	throws Xcept {

	    // set ?
	    if (! varSet(sd, v)) {
		Eqn eqn = new Eqn(sys, sd.expr, v, IExpr.EQ, expr.simplify());
		Util.verbose("adding eqn " + eqn);
		return;
	    }
	    Util.verbose("Variable not set,  was set previously " + v); 

	    // duplicate ?
	    if (force) throw new Xcept(v,
		"This variable is not user settable");
	}

	// compatible unit string
	abstract public String compatibleUnit();
	public Unit unit() { return unit; }

	// set unit
	public void setUnit(Unit u) throws Xcept {
	    Model model = getModel();
	    if (model.unitControl == Model.ON) {
		Unit cunit = model.parseUnit(compatibleUnit());
		if (! Unit.compatible(cunit, u)) throw new Xcept(this,
		    "Unit for component " + this.getClass().getName() +
		    " must be compatible with " + cunit.name());
		unit = u;
	    }
	}		

	// check/assign compatible units
	public void assignUnits() throws Xcept {
	    if (unit == null) setUnit(Unit.scalar());
	}			    

	// set all Chem concentrations mole/liter
	protected void assignConcUnits() throws Xcept {
	    for (int i=0; i<sys.chem.size(); i++) {
		Chem c = sys.chem.chem(i);
		setVarUnit(conc(c), c.unit());
	    }
	}

	// set child Var unit
	public void setVarUnit(Var v, Unit u) throws Xcept {
	    if (v.unit() == null) v.setUnit(u);
	    if (! Unit.compatible(v.unit(), u)) new Xcept(v,
		"Unit must be compatible with " + u);
	}
}

